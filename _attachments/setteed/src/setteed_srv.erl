%% 
-module(setteed_srv).
-behaviour(gen_server).

-export([ start_link/0, init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3 ]).

%% Main interface functions
-export([ start/0 ]).

-record(state, {
          couch_started = false :: atom()
         }).

%% Will remove once I figure out rebar releases
start() ->
    ok = inets:start(),
    ok = crypto:start(),
    {ok, _Pid} = reloader:start(),
    application:start(setteed).

-spec timeout() -> integer().
%% Amount of time in between polling RSS feeds, (if no activity has
%% happened)
timeout() ->
    timer:minutes(2).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{couch_started = false}, timer:seconds(0)}.

%% callbacks
handle_call(_Request, _From, State) ->
    {reply, ok, State, timeout()}.

handle_cast(_Msg, State) ->
    {noreply, State, timeout()}.

handle_info(_Info, #state{couch_started = false}) ->
    
    {ok, Host} = application:get_env(setteed, couch_host),
    {ok, Db}   = application:get_env(setteed, couch_db),
    Couch = dh_couch_srv:create_spec(Host, Db, fun notification/1),
    
    %% If this server has crashed, couch srv may already be started
    case supervisor:start_child(setteed_sup, Couch) of 
        {ok, _Pid}                       -> ok;
        {error, {already_started, _Pid}} -> ok
    end,
    
    {noreply, #state{couch_started = true}, timeout()};
handle_info(_Info, State) ->
    read_feeds(),
    {noreply, State, timeout()}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

feeds_expire() ->
    get_unix_timestamp(erlang:now()) - (1 * 60 * 10).

process_read_feeds() ->
    Fun = fun({_,_,Body}) ->
                  Res  = lists:foldl(fun save_to_doc/2, sets:new(),
                                     dh_json:get([<<"rows">>], Body)),
                  [ mark_processed(Id) || Id <- sets:to_list(Res) ]
          end,
    dh_couch_srv:view("items-to-process", Fun),
    ok.

save_to_doc(Item, Set) ->
    try
        ViewDoc = dh_json:get([<<"value">>], Item),        
        Id      = l2b(md5_hex(b2l(dh_json:get([<<"body">>], ViewDoc)))),
        Doc1 = dh_json:set([<<"read">>], false, ViewDoc),
        Doc2 = dh_json:set([<<"_id">>], Id, Doc1),
        Doc3 = dh_json:set([<<"type">>], <<"feeditem">>, Doc2),
        
        dh_couch_srv:save_doc(Doc3, fun(_X) -> ok end),        
        sets:add_element(dh_json:get([<<"sourceId">>], ViewDoc), Set)
    catch
        _Err:_ ->
            log("Error Processing ~p~n", [Item]),
            Set
    end.

mark_processed(Id) ->
    Fun = fun({_, _, Doc}) ->
                  Doc2 = dh_json:set([<<"processed">>], true, Doc),
                  dh_couch_srv:save_doc(Doc2, fun(_X) -> ok end)
          end,
    dh_couch_srv:doc(Id, Fun),
    ok.    

read_feeds() ->
    Fun = fun({_, _, Feeds}) ->
                  [ scrape_feed(Feed) ||
                      Feed <- dh_json:get([<<"rows">>], Feeds) ],
                  process_read_feeds()
          end,
    Params = [{"startkey", 0}, {"endkey", feeds_expire()}],
    dh_couch_srv:view("feeds-to-scrape", Params, Fun),
    ok.

scrape_feed(Feed) ->
    
    Time = get_unix_timestamp(erlang:now()),
    Id   = dh_json:get([<<"value">>, <<"_id">>], Feed),
    log("here"),
    Fun = fun(Data) ->
                  NStatus = case Data of
                                {error, no_scheme} ->                     
                                    {error, <<"Invalid Url">>};
                                {error, _} ->
                                    {error, <<"Unknown Error">>};
                                {_Url, _Hdrs, Body} ->
                                    NId = l2b(md5_hex(Body)),
                                    Doc = {struct, [ {<<"_id">>,     NId},
                                                     {<<"source">>,  Id},
                                                     {<<"type">>,    <<"batchfeed">>},
                                                     {<<"body">>,    l2b(Body)},
                                                     {<<"created">>, Time} ]},
                                    dh_couch_srv:save_doc(Doc, fun(_X) -> ok end),
                                    ok
                            end, 
                  
                  St = case NStatus of
                           ok ->
                               {struct, [{<<"text">>, <<"updated">>},
                                         {<<"cssClass">>, <<"ok">>}]};
                           {error, Reason} ->
                               {struct, [{<<"text">>, Reason},
                                         {<<"cssClass">>, <<"error">>}]}
                       end,
                  
                  Doc1 = dh_json:get([<<"value">>], Feed),    
                  Doc2 = dh_json:set([<<"status">>], St, Doc1),
                  Doc3 = dh_json:set([<<"processed">>], false, Doc2),
                  Doc4 = dh_json:set([<<"updated">>], Time, Doc3),
                  Doc5 = dh_json:remove([<<"_deleted_conflicts">>], Doc4),
                  
                  dh_couch_srv:save_doc(Doc5, fun(_X) -> ok end)
          end,
    dh_http:get(b2l(Id), [], [], Fun).


notification({_Url, _Hdrs, Json}) ->
    log("new feed!"),
    case trigger_read(dh_json:get([<<"results">>], Json)) of
        true  -> read_feeds();
        false -> ok
    end.

trigger_read([]) ->
    false;
trigger_read([Doc|Tail]) ->
    case dh_json:get([<<"doc">>, <<"type">>], Doc) == <<"feed">> of
        true  -> true;
        false -> trigger_read(Tail)
    end.

get_unix_timestamp(TS) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(TS)) -
        calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

md5_hex(S) ->
    Md5_bin  =  erlang:md5(S),
    Md5_list = b2l(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    [ int_to_hex(X) || X <- L ].

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).

log(Var) ->
    io:format("~p~n", [Var]).
log(Str, Args) ->
    io:format(Str, Args).

l2b(L) ->
    list_to_binary(L).
b2l(B) ->
    binary_to_list(B).

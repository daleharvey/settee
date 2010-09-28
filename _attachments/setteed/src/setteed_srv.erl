%% 
-module(setteed_srv).
-behaviour(gen_server).

-export([ start_link/0, init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3 ]).

%% Main interface functions
-export([ read_feeds/0,
          changes_subscribe/0,
          changes_unsubscribe/0 ]).

-record(state, {
          couch_opts  = undefined :: list(),
          changes_pid = undefined :: pid()
         }).

-spec timeout() -> integer().
%% Amount of time in between polling RSS feeds, (if no activity has
%% happened)
timeout() ->
    timer:minutes(2).

-spec read_feeds() -> term().
%% Just for command like debugging, invoke the reader
read_feeds() ->
    gen_server:call(?MODULE, read_feeds).

-spec changes_subscribe() -> term().
%% Listen to the changes feed from couch, so new feeds are read as soon
%% as they are added
changes_subscribe() ->
    gen_server:call(?MODULE, changes_subscribe).

-spec changes_unsubscribe() -> term().
%% Stop listening to the changes feed
changes_unsubscribe() ->
    gen_server:call(?MODULE, changes_unsubscribe).

-spec opts() -> list().
%% Options for connecting to couchdb, these should extracted into a config
%% file 
opts() ->
    [{host, "dale:tmppass@127.0.0.1:5984"},
     {db, "settee"}].

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{couch_opts = opts()}, timeout()}.

%% callbacks
handle_call(read_feeds, _From, State) ->
    read_feeds(State#state.couch_opts),
    {reply, ok, State, timeout()};

handle_call(changes_subscribe, _From, State) ->
    Opts  = State#state.couch_opts,
    Since = dh_json:get([<<"update_seq">>], dh_couch:db_info(Opts)),
    Fun   = fun(Data) -> notification(Opts, Data) end,
    Pid   = dh_couch:changes(Opts, Since, Fun),
    {reply, ok, State#state{changes_pid = Pid}, timeout()};

handle_call(changes_unsubscribe, _From, State) ->
    exit(State#state.changes_pid, normal),
    {reply, ok, State#state{changes_pid = undefined}, timeout()};    

handle_call(_Request, _From, State) ->
    {reply, ok, State, timeout()}.

handle_cast(_Msg, State) ->
    {noreply, State, timeout()}.

handle_info(_Info, State) ->
    read_feeds(State#state.couch_opts),
    {noreply, State, timeout()}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

feeds_expire() ->
    get_unix_timestamp(erlang:now()) - (1 * 60 * 10).

process_read_feeds(Opts) ->
    Items = dh_couch:view(Opts, "items-to-process"),
    Fun   = fun(X, Y) -> save_to_doc(Opts, X, Y) end,
    Res   = lists:foldl(Fun, sets:new(), dh_json:get([<<"rows">>], Items)),
    [ mark_processed(Opts, Id) || Id <- sets:to_list(Res) ],
    ok.

save_to_doc(Opts, Item, Set) ->
    try
        ViewDoc = dh_json:get([<<"value">>], Item),        
        Id      = l2b(md5_hex(b2l(dh_json:get([<<"body">>], ViewDoc)))),        
        Doc1 = dh_json:set([<<"read">>], false, ViewDoc),
        Doc2 = dh_json:set([<<"_id">>], Id, Doc1),
        Doc3 = dh_json:set([<<"type">>], <<"feeditem">>, Doc2),
        
        dh_couch:save_doc(Opts, Doc3),        
        sets:add_element(dh_json:get([<<"sourceId">>], ViewDoc), Set)
    catch
        _Err:_ ->
            log("Error Processing ~p~n", [Item]),
            Set
    end.

mark_processed(Opts, Id) ->
    Doc  = dh_couch:doc(Opts, Id),
    Doc2 = dh_json:set([<<"processed">>], true, Doc),
    dh_couch:save_doc(Opts, Doc2),
    ok.    
    
read_feeds(Opts) ->
    Params = [{"startkey", 0}, {"endkey", feeds_expire()}],
    Feeds = dh_couch:view(Opts, "feeds-to-scrape", Params),
    [ scrape_feed(Opts, Feed) || Feed <- dh_json:get([<<"rows">>], Feeds) ],
    process_read_feeds(Opts).

scrape_feed(Opts, Feed) ->
    
    Time = get_unix_timestamp(erlang:now()),
    Id   = dh_json:get([<<"value">>, <<"_id">>], Feed),
    Url  = binary_to_list(Id),

    NStatus = case dh_http:get(Url) of
                  {error, no_scheme} ->                     
                      {error, <<"InvalidUrl">>};
                  {error, _} ->
                      {error, <<"UnknownError">>};
                  {_Url, _Hdrs, Body} ->
                      NId = l2b(md5_hex(Body)),
                      Doc = {struct, [ {<<"_id">>,     NId},
                                       {<<"source">>,  Id},
                                       {<<"type">>,    <<"batchfeed">>},
                                       {<<"body">>,    l2b(Body)},
                                       {<<"created">>, Time} ]},
                      dh_couch:save_doc(Opts, Doc),
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
    
    ok = dh_couch:save_doc(Opts, Doc5).

notification(Opts, {_Url, _Hdrs, Json}) ->
    case trigger_read(dh_json:get([<<"results">>], Json)) of
        true  -> read_feeds(Opts);
        false -> io:format("ignoring~n"),
                 ok
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
    Md5_list = binary_to_list(Md5_bin),
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

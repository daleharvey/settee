%% couchapi, will mostly
%% be ugly plain calls for now but will extract a nice flexible api sometime
-module(dh_couch_srv).
-behaviour(gen_server).

-export([ start_link/4, init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3 ]).

-export([ create_spec/3, create_spec/4,
          db_info/1, db_info/2, 
          view/2, view/3, view/4,
          doc/2, doc/3,
          save_doc/2, save_doc/3 ]).

-type json() :: tuple().

-record(state, {
          host            = undefined :: list(),
          db              = undefined :: list(),
          changes_started = undefined :: pid(),
          changes_cb      = undefined :: fun() 
         }).


%%% interface

-spec create_spec(list(), list(), fun()) -> tuple().
%% Generate a child specification to make it easier to add to supervision
%% trees dynamically
create_spec(Host, Db, Changes) ->
    create_spec(Host, Db, Changes, ?MODULE).

create_spec(Host, Db, Changes, Name) ->
    {?MODULE, {?MODULE, start_link, [Name, Host, Db, Changes]}, permanent,
     5000, worker, [?MODULE]}.

start_link(Name, Host, Db, Changes) ->
    gen_server:start_link({local, Name}, ?MODULE, [Host, Db, Changes], []).

-spec doc(atom(), list()) -> ok.
%% Fetch a document
doc(Id, Callback) ->
    doc(?MODULE, Id, Callback).
doc(Name, Id, Callback) ->
    gen_server:call(Name, {doc, Id, Callback}).


-spec save_doc(json(), fun()) -> ok.
%% Save a document
save_doc(Doc, Callback) ->
    save_doc(?MODULE, Doc, Callback).
save_doc(Name, Doc, Callback) ->
    gen_server:call(Name, {save_doc, Doc, Callback}).

-spec db_info(fun()) -> ok.
%% Pull information about a db
db_info(Callback) ->
    db_info(?MODULE, Callback).
db_info(Name, Callback) ->
    gen_server:call(Name, {db_info, Callback}).

-spec view(list(), fun()) -> ok.
%% Read a db view
view(ViewName, Callback) ->
    view(?MODULE, ViewName, [], Callback).
view(ViewName, Params, Callback) ->
    view(?MODULE, ViewName, Params, Callback).
view(Name, ViewName, Params, Callback) ->
    gen_server:call(Name, {view, ViewName, Params, Callback}).

%%% Callbacks
-spec init(list()) -> term().
init([Host, Db, Changes]) ->
    process_flag(trap_exit, true),
    {ok, #state{host = Host, db = Db, changes_cb = Changes,
                changes_started = false}, timer:seconds(0)}.

-spec handle_call(term(), pid(), #state{}) -> term().
handle_call({doc, Id, Cb}, _From, #state{host=Host, db=Db}=State) ->
    {reply, doc(Host, Db, Id, Cb), State};
handle_call({save_doc, Doc, Cb}, _From, #state{host=Host, db=Db}=State) ->
    {reply, save_doc(Host, Db, Doc, Cb), State};
handle_call({db_info, Cb}, _From, #state{host=Host, db=Db}=State) ->
    {reply, db_info(Host, Db, Cb), State};
handle_call({view, Name, Param, Cb}, _From, #state{host=Host, db=Db}=State) ->
    {reply, view(Host, Db, Name, Param, Cb), State}.

-spec handle_cast(term(), #state{}) -> term().
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> term().
%% Set to automatically start the changes feed when the server starts
%% useful for supervised servers
handle_info(_Info, #state{changes_started=false}=State) ->
    #state{host=Host, db=Db, changes_cb=Cb} = State,
    ok = changes(Host, Db, Cb),
    {noreply, State#state{changes_started = true}};

%% The fun that long polls will be killed from time to time due to
%% recompilation, restart it when that happens
handle_info({'EXIT', _Pid, killed}, State) ->
    #state{host=Host, db=Db, changes_cb=Cb} = State,
    ok = changes(Host, Db, Cb),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% callbacks
-spec doc(list(), list(), list(), fun()) -> ok.
doc(Host, Db, Id, Cb) ->
    Url = str("http://~s/~s/~s", [Host, Db, dh_http:url_encode(Id)]),
    dh_http:get(Url, [], [{type, json}], Cb).

-spec db_info(list(), list(), fun()) -> ok.
db_info(Host, Db, Cb) ->
    Url = str("http://~s/~s/", [Host, Db]),
    dh_http:get(Url, [], [{type, json}], Cb).

-spec view(list(), list(), string(), list(), fun()) -> ok.
view(Host, Db, Name, Params, Cb) ->
    Url = str("http://~s/~s/_design/~s/_view/~s", [Host, Db, Db, Name]),
    dh_http:get(Url, Params, [{type, json}], Cb).

-spec save_doc(list(), list(), json(), fun()) -> ok.
save_doc(Host, Db, Doc, Cb) ->
    Id  = dh_json:get([<<"_id">>], Doc), 
    Url = str("http://~s/~s/~s", [Host, Db, dh_http:url_encode(Id)]),    
    dh_http:put(Url, Doc, [{type, json}], Cb).

-spec changes(list(), list(), fun()) -> ok.
changes(Host, Db, Cb) ->
    Srv = self(),
    Fun = fun({_, _, Body}) ->
                  Url = str("http://~s/~s/_changes", [Host, Db]),
                  Fun = fun(Data) -> db_changed(Host, Db, Srv, Data, Cb) end,
                  Params = params(dh_json:get([<<"update_seq">>], Body)),
                  dh_http:get(Url, Params, [{type, json}, {link, Srv}], Fun)
          end,
    db_info(Host, Db, Fun).

db_changed(Host, Db, Srv, {_, _, Json} = Res, Callback) ->
    
    try   Callback(Res)
    catch Type:Err ->
            Stack = erlang:get_stacktrace(),
            io:format("Type: ~p~nError: ~p~nStack: ~p~n", [Type, Err, Stack])
    end,
    
    Url    = str("http://~s/~s/_changes", [Host, Db]),
    Params = params(dh_json:get([<<"last_seq">>], Json)),
    Fun    = fun(X) -> db_changed(Host, Db, Srv, X, Callback) end,
    dh_http:get(Url, Params, [{type, json}, {link, Srv}], Fun).

-spec params(integer()) -> list().
params(Since) ->
    [{"since",        Since},
     {"include_docs", true},
     {"feed",         "longpoll"},
     {"heartbeat",    10000}].

str(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).

%% couchapi, will mostly
%% be ugly plain calls for now but will extract a nice flexible api sometime
-module(dh_couch_srv).
-behaviour(gen_server).

-export([ start_link/4, init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3 ]).

-export([ create_spec/3, create_spec/4,
          db_info/0, db_info/1, 
          view/1, view/2, view/3,
          %subscribe_changes/2, unsubscribe_changes/1,
          doc/1, doc/2,
          save_doc/1, save_doc/2,
          stop/1 ]).

-type json() :: tuple().

-record(state, {
          host        = undefined :: list(),
          db          = undefined :: list(),
          changes_pid = undefined :: pid(),
          changes_cb  = undefined :: fun() 
         }).

%%% interface
-spec create_spec(list(), list(), atom()) -> tuple().
%% Generate a child specification to make it easier to add to supervision
%% trees dynamically
create_spec(Host, Db, Changes) ->
    create_spec(Host, Db, Changes, ?MODULE).

create_spec(Host, Db, Changes, Name) ->
    {?MODULE, {?MODULE, start_link, [Name, Host, Db, Changes]}, permanent, 5000,
     worker, [?MODULE]}.

start_link(Name, Host, Db, Changes) ->
    gen_server:start_link({local, Name}, ?MODULE, [Host, Db, Changes], []).

-spec doc(atom(), list()) -> json().
%% Fetch a document
doc(Id) ->
    doc(?MODULE, Id).
doc(Name, Id) ->
    gen_server:call(Name, {doc, Id}).


-spec save_doc(json()) -> ok | {error, term()}.
%% Save a document
save_doc(Doc) ->
    save_doc(?MODULE, Doc).
save_doc(Name, Doc) ->
    gen_server:call(Name, {save_doc, Doc}).
 

db_info() ->
    db_info(?MODULE).
db_info(Name) ->
    gen_server:call(Name, db_info).

view(ViewName) ->
    view(?MODULE, ViewName).
view(Name, ViewName) ->
    gen_server:call(Name, {view, ViewName, []});
view(ViewName, Params) ->
    view(?MODULE, ViewName, Params).
view(Name, ViewName, Params) -> 
   gen_server:call(Name, {view, ViewName, Params}).

stop(Name) ->
    gen_server:cast(Name, stop).

%% callbacks
init([Host, Db, Changes]) ->
    process_flag(trap_exit, true),
    {ok, #state{host = Host, db = Db, changes_cb = Changes}, timer:seconds(0)}.

handle_call({doc, Id}, _From, #state{host=Host, db=Db}=State) ->
    {reply, doc(Host, Db, Id), State};
handle_call({save_doc, Doc}, _From, #state{host=Host, db = Db}=State) ->
    {reply, save_doc(Host, Db, Doc), State};
handle_call(db_info, _From, #state{host=Host, db = Db}=State) ->
    {reply, db_info(Host, Db), State};
handle_call({view, Name, Params}, _From, #state{host=Host, db = Db}=State) ->
    io:format("got here~n"),
    {reply, view(Host, Db, Name, Params), State};
handle_call({subscribe_changes, Cb}, _From, #state{host=Host, db=Db}=S) ->
    Pid = changes(Host, Db, Cb),
    {reply, ok, S#state{changes_pid = Pid, changes_cb = Cb}};
handle_call(unsubscribe_changes, _From, State) ->
    exit(State#state.changes_pid, normal),
    {reply, ok, State#state{changes_pid = undefined, changes_cb = undefined}};

handle_call(_Request, _From, State) ->
    io:format("~p~n", [_Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Set to automatically start the changes feed when the server starts
%% useful for supervised servers
handle_info(_Info, #state{changes_cb = Cb, changes_pid = Pid} = State)
  when Cb =/= undefined andalso Pid =:= undefined ->
    #state{host = Host, db = Db} = State,
    NewPid = changes(Host, Db, Cb),
    io:format("completes~n"),
    {noreply, State#state{changes_pid = NewPid}};

%% The fun that long polls will be killed from time to time due to
%% recompilation, restart it when that happens
handle_info({'EXIT', Pid, killed}, #state{changes_pid=Pid} = State) ->
    #state{host=Host, db=Db, changes_pid=Pid, changes_cb=Cb} = State,
    NewPid = changes(Host, Db, Cb),
    io:format("completes~n"),
    {noreply, #state{changes_pid = NewPid}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% callbacks
doc(Host, Db, Id) ->
    Url = str("http://~s/~s/~s", [Host, Db, dh_http:url_encode(Id)]),
    {_Url, _Hdrs, Body} = dh_http:get(Url, [], [{type, json}]),
    Body.

db_info(Host, Db) ->
    Url = str("http://~s/~s/", [Host, Db]),
    {_Url, _Hdrs, Body} = dh_http:get(Url, [], [{type, json}]),
    Body.

view(Host, Db, Name, Params) ->
    Url = str("http://~s/~s/_design/~s/_view/~s", [Host, Db, Db, Name]),
    io:format("fetching ~p~n",[Url]),
    {_Url, _Hdrs, Body} = dh_http:get(Url, Params, [{type, json}]),
    Body.

save_doc(Host, Db, Doc) ->
    Id  = dh_json:get([<<"_id">>], Doc), 
    Url = str("http://~s/~s/~s", [Host, Db, dh_http:url_encode(Id)]),    
    dh_http:put(Url, Doc, [{type, json}]).

%% This process will be killed by the vm on recompile
%% need to persist
changes(Host, Db, Callback) ->
    io:format("changes starts~n"),
    Since = dh_json:get([<<"update_seq">>], db_info(Host, Db)),
    changes(Host, Db, Since, Callback).

changes(Host, Db, Since, Callback) ->
    Req = fun(Self, TSince) ->
                  
                  Params = [{"since", TSince},
                            {"include_docs", true},
                            {"feed", "longpoll"},
                            {"heartbeat", 10000}],
                  
                  Url = str("http://~s/~s/_changes", [Host, Db]),
                  Res = {_Url, _Hdrs, Body}
                      = dh_http:get(Url, Params, [{type, json}]),
                  
                  try   Callback(Res)
                  catch Type:Err ->
                          io:format("Type: ~p~nError: ~p~nStack: ~p~n",
                                    [Type, Err, erlang:get_stacktrace()]),
                          ok
                  end,
                  
                  Self(Self, dh_json:get([<<"last_seq">>], Body))
          end,

    Pid = spawn_link( fun() -> Req(Req, Since) end ),
    io:format("changes ends~n"),
    Pid.

str(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).

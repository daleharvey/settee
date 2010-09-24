%% 
-module(setteed_srv).
-behaviour(gen_server).

%% Main interface functions
-export([ changes_subscribe/1,
          changes_unsubscribe/0
         ]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          changes_pid = undefined :: pid()
         }).

-spec timeout() -> integer().
timeout() ->
    timer:minutes(15).

db_notify(Msg) ->
    gen_server:call(?MODULE, {db_notify, Msg}).

changes_subscribe(Opts) ->
    gen_server:call(?MODULE, {change_subscribe, Opts}).
changes_unsubscribe() ->
    gen_server:call(?MODULE, change_unsubscribe).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}, timeout()}.

%% callbacks
handle_call({db_notify, {_Url, _Hdrs, _Body}}, _From, State) ->
    {reply, ok, State, timeout()};

handle_call({changes_subscribe, Opts}, _From, State) ->
    Since = dh_json:get([<<"update_seq">>], dh_couch:db_info(Opts)),
    Pid   = dh_couch:changes(Opts(), Since, fun notification/1),
    {reply, ok, State#state{changes_pid = Pid}, timeout()};

handle_call(changes_unsubscribe, _From, State) ->
    exit(State#state.changes_pid, normal),
    {reply, ok, State#state{changes_pid = undefined}, timeout()};    

handle_call(_Request, _From, State) ->
    {reply, ok, State, timeout()}.

handle_cast(_Msg, State) ->
    {noreply, State, timeout()}.

handle_info(_Info, State) ->
    %% update(),
    {noreply, State, timeout()}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

notification(Msg) ->
    io:format("~p~n", [Msg]).
    

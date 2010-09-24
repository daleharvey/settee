-module(settee).
-compile(export_all).

%% tmp until I figure out rebar release stuff
init() ->
    ok = inets:start(),
    ok = crypto:start(),
    {ok, _Pid} = reloader:start(),
    application:start(setteed).

couch_opts() ->
    [{host, "dale:hail99@127.0.0.1:5984"},
     {db, "settee"}].

bleh() ->
    Fun = fun(Res) -> setteed_srv:db_notify(Res) end,
    Since = dh_json:get([<<"update_seq">>], dh_couch:db_info(couch_opts())),
    Pid   = dh_couch:changes(couch_opts(), Since, Fun),
    Pid.

%% Lets make a couchdb client as well

%% Utils
f(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.


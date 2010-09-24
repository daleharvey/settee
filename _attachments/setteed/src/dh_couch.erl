-module(dh_couch).

-export([db_info/1, changes/3]).

db_info(Opts) ->
    Url = str("http://~s/~s/", [f(host, Opts), f(db, Opts)]),
    {_Url, _Hdrs, Body} = dh_http:get(Url, [{type, json}]),
    Body.
    

changes(Opts, Since, Callback) ->
    
    Req = fun(Self, TSince) ->
                  
                  Params = [{"since", TSince},
                            {"include_docs", true},
                            {"feed", "longpoll"},
                            {"heartbeat", 10000}],
                  
                  Url = str("http://~s/~s/_changes",
                            [f(host, Opts), f(db, Opts)]),
                  Res = {_Url, _Hdrs, Body} = dh_http:get(Url,
                                                          [{params, Params},
                                                           {type, json}]),
                  NSince = dh_json:get([<<"last_seq">>], Body),
                  Callback(Res),
                  Self(Self, NSince)
          end,
    
    spawn( fun() -> Req(Req, Since) end ).

%% Utils
f(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.

str(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).

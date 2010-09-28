%% couchapi, will mostly
%% be ugly plain calls for now but will extract a nice flexible api sometime
-module(dh_couch).

-export([ db_info/1,
          view/2, view/3,
          changes/3,
          doc/2,
          save_doc/2 ]).

db_info(Opts) ->
    Url = str("http://~s/~s/", [f(host, Opts), f(db, Opts)]),
    {_Url, _Hdrs, Body} = dh_http:get(Url, [], [{type, json}]),
    Body.

view(Opts, Name) ->
    view(Opts, Name, []).
view(Opts, Name, Params) ->
    Url = str("http://~s/~s/_design/~s/_view/~s",
              [f(host, Opts), f(db, Opts), f(db, Opts), Name]),
    {_Url, _Hdrs, Body} = dh_http:get(Url, Params, [{type, json}]),
    Body.

doc(Opts, Id) ->
    Url = str("http://~s/~s/~s", [f(host, Opts), f(db, Opts),
                                  dh_http:url_encode(Id)]),
    {_Url, _Hdrs, Body} = dh_http:get(Url, [], [{type, json}]),
    Body.

save_doc(Opts, Doc) ->
    Id  = dh_json:get([<<"_id">>], Doc), 
    Url = str("http://~s/~s/~s", [f(host, Opts), f(db, Opts),
                                  dh_http:url_encode(Id)]),    
    dh_http:put(Url, Doc, [{type, json}]).

%% This process will be killed by the vm on recompile
%% need to persist
changes(Opts, Since, Callback) ->
    
    Req = fun(Self, TSince) ->
                  
                  Params = [{"since", TSince},
                            {"include_docs", true},
                            {"feed", "longpoll"},
                            {"heartbeat", 10000}],
                  
                  Url = str("http://~s/~s/_changes",
                            [f(host, Opts), f(db, Opts)]),
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
    
    spawn( fun() -> Req(Req, Since) end ).

%% Utils
f(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.

str(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).

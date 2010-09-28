%% Lets make a http api that doesnt suck for erlang, will mostly
%% be ugly plain calls for now but will extract a nice flexible api sometime
% get("http://google.com").
% get("http://google.com", [{"q", "test"}])
-module(dh_http).
-export([ get/1, get/2, get/3,
          put/3,
          url_encode/1 ]).

get(Url) ->
    get(Url, []).
get(Url, Params) ->
    get(Url, Params, []).
get(Url, Params, Opts) ->
    {NewUrl, _Headers, _Body}
        = prehttp([{get_params, Params} | Opts], {Url, [], []}),
    case httpc:request(NewUrl) of
        {ok, {{_, 200, "OK"}, NHeaders, NBody}} ->
            posthttp(Opts, {NewUrl, NHeaders, NBody});
        Else -> Else
    end.

put(Url, Body, _Opts) ->

    Body1 = lists:flatten(io_lib:format("~s", [mochijson2:encode(Body)])),
    NOpts = {Url, [], "application/json", Body1},
    
    case httpc:request(put, NOpts, [], []) of
        {ok, {{_, 201, _}, _, _}} ->
            ok;
        Else ->
            Else
    end.

url_encode(Str) ->
    Str1 = re:replace(Str, " ", "%20", [global, {return, list}]),
    re:replace(Str1, "/", "%2F", [global, {return, list}]).    
    
param_to_str({Key, Val}) ->
    Key ++ "=" ++ to_str(Val).

pret({get_params, List}, {Url, Headers, Body}) ->
    NewUrl = Url ++ "?" ++
        string:join([param_to_str(X) || X <- List], "&"),
    {NewUrl, Headers, Body};
pret(_, Data) ->
    Data.

prehttp(Opts, Data) ->
    lists:foldl(fun pret/2, Data, Opts).

postt({type, json}, {Url, Headers, Body}) ->
    {Url, Headers, mochijson2:decode(Body)};
postt(_, Data) ->
    Data.

posthttp(Opts, Data) ->
    lists:foldl(fun postt/2, Data, Opts).

to_str(X) when is_integer(X) ->
    integer_to_list(X);
to_str(X) when is_atom(X) ->
    atom_to_list(X);
to_str(X) ->
    X.

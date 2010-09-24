-module(dh_http).
-export([get/1, get/2]).

%% Lets make a http api that doesnt suck for erlang
% get("http://google.com").
% get("http://google.com", [{params, [{"q", "test"}]}]).
param_to_str({Key, Val}) ->
    Key ++ "=" ++ to_str(Val).

pret({params, List}, {Url, Headers, Body}) ->
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

get(Url) ->
    get(Url, []).
get(Url, Opts) ->
    {NewUrl, _Headers, _Body} = prehttp(Opts, {Url, [], []}),
    {ok, {{_, 200, "OK"}, NHeaders, NBody}} = httpc:request(NewUrl),    
    posthttp(Opts, {NewUrl, NHeaders, NBody}).

to_str(X) when is_integer(X) ->
    integer_to_list(X);
to_str(X) when is_atom(X) ->
    atom_to_list(X);
to_str(X) ->
    X.

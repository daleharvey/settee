-module(dh_json).
-export([get/2]).

get([Key], {struct, Vals}) ->
    case lists:keyfind(Key, 1, Vals) of
        false -> undefined;
        {Key, Val} -> Val
    end;

get([Key | Tail], {struct, Vals}) ->
    case lists:keyfind(Key, 1, Vals) of
        false -> undefined;
        {Key, Val} -> get(Tail, Val)
    end.

%% Utils
f(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.


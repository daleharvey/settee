-module(dh_json).
-export([ get/2, set/3, remove/2 ]).

set([H], Val, {struct, Obj}) ->
    Vals = case lists:keyfind(H, 1, Obj) of
               false -> [{H, Val} | Obj];
               _Else -> lists:keyreplace(H, 1, Obj, {H, Val})
           end,
    {struct, Vals};
set([H|T], Val, {struct, Obj}) ->
    Vals = case lists:keyfind(H, 1, Obj) of
               false ->
                   [{H, [set(T, Val, {struct, []})]} | Obj];
               {H, Child} ->
                   lists:keyreplace(H, 1, Obj, set(T, Val, Child))
           end,
    {struct, Vals}.    

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

remove([Key], {struct, Vals}) ->
    {struct, [ {K,V} || {K,V} <- Vals, K =/= Key ]};
remove([Key|Tail], {struct, Vals}) ->
    {struct, lists:keyreplace(Key, 1, Vals, remove(Tail, f(Key, Vals)))}.
    

%% Utils
f(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.


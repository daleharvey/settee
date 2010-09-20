#!/usr/bin/env escript
%% -*- erlang -*-

couch() ->
    "http://dale:hail99@127.0.0.1:5984".
db() ->
    "settee".
view() ->
    "items-to-process".

main([]) ->

    ok = inets:start(),
    ok = crypto:start(),

    Views = "/" ++ db() ++ "/_design/settee/_view/",
    
    {ok, {_St, _Hdrs, Body}} = httpc:request(couch() ++ Views ++ view()),

    {struct, JSON} = mochijson:decode(Body),
    {"rows", {array, Feeds}} = lists:keyfind("rows", 1, JSON),

    Res = lists:foldl(fun(X, Y) -> processFeed(X, Y) end, sets:new(), Feeds),
    io:format("Yay ~p~n", [sets:to_list(Res)]),
    ok.

processFeed({struct, Item}, Set) ->
    
    try
        {"id", Id} = lists:keyfind("id", 1, Item),
        {"value", Old} = lists:keyfind("value", 1, Item),
        
        {struct, Vals} = Old,
        {"body", Body} = lists:keyfind("body", 1, Vals),
        NewId = md5_hex(Body),
        
        NewDoc1 = json_set(["read"], "false", Old),
        NewDoc2 = json_set(["_id"], NewId, NewDoc1),
        NewDoc3 = json_set(["type"], "feeditem", NewDoc2),
        
        Opts = {couch() ++ "/" ++ db() ++ "/" ++ NewId, [],
                "application/json",
                lists:flatten(mochijson:encode(NewDoc3))},
        
        case httpc:request(put, Opts, [], []) of 
            {ok, {{_, 201, _}, _, _}} ->   
                io:format("Saved ~p ~n", [NewId]);
            {ok, {{_, 409, _}, _, _}} ->
                io:format("Duplicate ~p ~n",[NewId])
        end,                     
        
        sets:add_element(Id, Set)
        
    catch
        _Err:_ ->
            io:format("Error Processing ~p~n", [Item]),
            Set
    end.

json_set([H], Val, {struct, Obj}) when is_list(H) ->
    Vals = case lists:keyfind(H, 1, Obj) of
               false -> [{H, Val} | Obj];
               _Else -> lists:keyreplace(H, 1, Obj, {H, Val})
           end,
    {struct, Vals};
json_set([H|T], Val, {struct, Obj}) when is_list(H) ->
    Vals = case lists:keyfind(H, 1, Obj) of
               false ->
                   [{H, [json_set(T, Val, {struct, []})]} | Obj];
               {H, Child} ->
                   lists:keyreplace(H, 1, Obj, json_set(T, Val, Child))
           end,
    {struct, Vals}.    

md5_hex(S) ->
    Md5_bin  = erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    [ int_to_hex(X) || X <- L ].

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).

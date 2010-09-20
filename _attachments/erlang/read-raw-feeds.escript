#!/usr/bin/env escript
%% -*- erlang -*-

couch() ->
    "http://dale:hail99@127.0.0.1:5984".
db() ->
    "settee".
view() ->
    "feeds-to-scrape".
    
main([]) ->

    ok = inets:start(),
    ok = crypto:start(),

    Views = "/" ++ db() ++ "/_design/settee/_view/",
    
    {ok, {_St, _Hdrs, Body}} = httpc:request(couch() ++ Views ++ view()),

    {struct, JSON} = mochijson:decode(Body),
    {"rows", {array, Feeds}} = lists:keyfind("rows", 1, JSON),
    
    [ processFeed(Item) || {struct, Item} <- Feeds].

processFeed(Item) ->

    {"id", RawUrl} = lists:keyfind("id", 1, Item),
    {"value", Old} = lists:keyfind("value", 1, Item),
    Url = re:replace(RawUrl, "/", "%2F", [global, {return, list}]),
    
    io:format("Fetching: ~p~n", [RawUrl]),
    
    Result = case catch httpc:request(RawUrl) of
                 {error, no_scheme} ->
                     {error, "invalid_url"};
                 {error, _} ->
                     {error, "uknown error"};
                 {ok, {_St, _Hdrs, Tmp}} ->
                     {ok, Tmp}
             end,                 

    Time = get_unix_timestamp(erlang:now()),
    
    {Text, CSS}=case Result of
                    {error, Reason} ->
                        io:format("Error: ~p~n", [Reason]),
                        {"Url could not be read", "error"};
                    {ok, Body} ->

                        Id = md5_hex(Body),
                        Doc = {struct, [{"_id", Id},
                                        {"source", RawUrl},
                                        {"type", "batchfeed"},
                                        {"body", Body}, {"created", Time}]},

                        Opts = {couch() ++ "/" ++ db() ++ "/" ++ Id, [],
                                "application/json",
                                lists:flatten(mochijson:encode(Doc))},

                        io:format("Saving ~p ~n",[Id]),

                        case httpc:request(put, Opts, [], []) of 
                            {ok, {{_, 201, _}, _, _}} ->   
                                io:format("Saved ~p ~n",[Id]);
                            {ok, {{_, 409, _}, _, _}} ->
                                io:format("Duplicate ~p ~n",[Id])
                        end,                     
                        {"updated", "ok"}
                end,

    Status = {struct, [{"text", Text}, {"cssClass", CSS}]},
    NewDoc1 = json_set(["status"], Status, Old),
    NewDoc2 = json_set(["processed"], "false", NewDoc1),
    NewDoc3 = json_set(["updated"], Time, NewDoc2),
    NewDoc4 = json_set(["_deleted_conflicts"], {array, []}, NewDoc3),
    
    NOpts = {couch() ++ "/" ++ db() ++ "/" ++ Url, [],
             "application/json",
             lists:flatten(mochijson:encode(NewDoc4))},
    
    {ok, {{_, 201, _}, _, _}} = httpc:request(put, NOpts, [], []),
    io:format("Updated Status ~n"),
    ok.

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

get_unix_timestamp(TS) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(TS)) -
        calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

md5_hex(S) ->
    Md5_bin  =  erlang:md5(S),
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

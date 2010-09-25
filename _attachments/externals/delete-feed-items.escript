#!/usr/bin/env escript
%% -*- erlang -*-

couch() ->
    "http://dale:hail99@127.0.0.1:5984".
db() ->
    "settee".
view() ->
    "feed-listing".
    
main([]) ->

    ok = inets:start(),
    ok = crypto:start(),

    Views = "/" ++ db() ++ "/_design/settee/_view/",
    
    {ok, {_St, _Hdrs, Body}} = httpc:request(couch() ++ Views ++ view()),

    {struct, JSON} = mochijson:decode(Body),
    {"rows", {array, Feeds}} = lists:keyfind("rows", 1, JSON),
    
    [ processFeed(Item) || {struct, Item} <- Feeds].

processFeed(Item) ->

    {"value", {struct, Doc}} = lists:keyfind("value", 1, Item),

    {"_id", Id} = lists:keyfind("_id", 1, Doc),
    {"_rev", Rev} = lists:keyfind("_rev", 1, Doc),

    Opts = {couch() ++ "/" ++ db() ++ "/" ++ Id ++ "?rev=" ++ Rev, []},
    
    {ok, {{_, 200, _}, _, _Result}} = httpc:request(delete, Opts, [], []),

    ok.

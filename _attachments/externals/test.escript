#!/usr/bin/env escript
%% -*- erlang -*-

logfile() ->
    "/home/dale/tmp/couchdberl.log".
externals() ->
    "./_attachments/externals".

log(File, Str) ->
    log(File, Str, []).
log(File, Str, Args) ->
    ok = file:write(File, io_lib:format(Str, Args)).

run_update(Log) ->    
    log(Log, os:cmd(externals() ++ "/read-raw-feeds.escript")),
    log(Log, os:cmd(externals() ++ "/from-view-to-docs.escript")),
    ok.

read(Log, Time) ->
    
    Read = io:get_line(""),
    
    case Read of
        eof -> ok;
        Else ->
            
            log(Log, "Read ~s~n", [Read]),

            case get_unix_timestamp(erlang:now()) - Time of
                X when X > 1 -> run_update(Log);
                Else         -> log(Log, "Skipping Update~p")
            end,            
                               
            read(Log, get_unix_timestamp(erlang:now()))
    end.

main(Args) ->

    {ok, Log} = file:open(logfile(), [append]),

    log(Log, "Started from ~p with Args ~p~n", [file:get_cwd(), Args]),
    
    read(Log, get_unix_timestamp(erlang:now())),

    log(Log, "Exited normally ~n"),
    file:close(Log),
    
    ok.

get_unix_timestamp(TS) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(TS)) -
        calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

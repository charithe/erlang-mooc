-module(mailbox).
-export([receiver/0, recv_ordered/0]).

receiver() ->
    receive
        Msg -> 
            io:format("GOT: ~w~n", [Msg]),
            timer:sleep(1000), 
            receiver()
    end.

recv_ordered() ->
    recv(first),
    recv(second).

recv(Pat) ->
    receive
        stop -> io:format("Stopping~n");
        {Pat, Value} -> io:format("~s~n",[Value]);
        _ -> recv(Pat)
    end.


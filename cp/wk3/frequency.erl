%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,inject/1,stop/0]).
-export([frontend/0, frontend_loop/1, init/1, loop/1]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(?MODULE,
             spawn(?MODULE, frontend, [])).

frontend() ->
    process_flag(trap_exit, true),
    S1 = spawn_link(?MODULE, init, [lists:seq(10, 15)]),
    S2 = spawn_link(?MODULE, init, [lists:seq(20, 25)]),
    frontend_loop([{S1, 10, 15, 0}, {S2, 20, 25, 0}]).


frontend_loop(Servers) ->
    receive
        {request, Pid, allocate} ->
            Server = pick_free_server(Servers),
            Server ! {request, Pid, allocate},
            NewServers = mark_server_allocation(Servers, Server),
            io:format("~w~n", [NewServers]),
            ?MODULE:frontend_loop(NewServers);
        {request, Pid, {deallocate, Freq}} ->
            Server = find_server(Servers, Freq),
            Server ! {request, Pid, {deallocate, Freq}},
            NewServers = mark_server_allocation(Servers, Server),
            io:format("~w~n", [NewServers]),
            ?MODULE:frontend_loop(NewServers);
        {request, Pid, stop} ->
            lists:foreach(fun({S, _, _, _}) -> S ! {request, Pid, stop} end, Servers),
            Pid ! {reply, stopped}
    end.

pick_free_server(Servers) ->
    {S, _, _, _} = lists:last(lists:sort(fun({_, _, _, NumReq1}, {_, _, _, NumReq2}) -> NumReq1 > NumReq2 end, Servers)),
    S.

find_server([], _) -> throw(illegal_state);
find_server([{S, Min, Max, _}|_], Freq) when (Min =< Freq) and (Freq =< Max) -> S;
find_server([_|T], Freq) -> find_server(T, Freq).


mark_server_allocation(Servers, S) -> 
    {S, Min, Max, NumReq} = lists:keyfind(S, 1, Servers),
    lists:keyreplace(S, 1, Servers, {S, Min, Max, NumReq + 1}).
    

init(FreqList) ->
    Frequencies = {FreqList, []},
    loop(Frequencies).


%% The Main Loop

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            ?MODULE:loop(NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            Pid ! {reply, ok},
            ?MODULE:loop(NewFrequencies);
        {request, Pid, {inject, Freqs}} -> 
            NewFrequencies = inject(Frequencies, Freqs),
            Pid ! {reply, ok},
            ?MODULE:loop(NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end.

%% Functional interface

allocate() -> 
    ?MODULE ! {request, self(), allocate},
    receive 
        {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    ?MODULE ! {request, self(), {deallocate, Freq}},
    receive 
        {reply, Reply} -> Reply
    end.

inject(Freqs) ->
    ?MODULE ! {request, self(), {inject, Freqs}},
    receive
        {reply, Reply} -> Reply
    end.

stop() -> 
    ?MODULE ! {request, self(), stop},
    receive 
        {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free],  NewAllocated}.

inject({Free, Allocated}, Freqs) ->
    {Free ++ Freqs, Allocated}.

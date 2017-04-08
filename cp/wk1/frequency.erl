%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
             spawn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            sleep_random(),
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            sleep_random(),
            {NewFrequencies, Reply} = deallocate(Frequencies, {Freq, Pid}),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end.

%% Functional interface

allocate() -> 
    clear(),
    frequency ! {request, self(), allocate},
    receive 
        {reply, Reply} -> Reply
    after 100 -> timeout
    end.

deallocate(Freq) -> 
    clear(),
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
        {reply, Reply} -> Reply
    after 100 -> timeout
    end.

stop() -> 
    clear(),
    frequency ! {request, self(), stop},
    receive 
        {reply, Reply} -> Reply
    after 100 -> timeout
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    case lists:keyfind(Pid, 2, Allocated) of
        false -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
        _ -> {{[Freq|Free], Allocated }, error}
    end.

deallocate({Free, Allocated}, {Freq, Pid}) ->
    case lists:keyfind(Freq, 1, Allocated) of
        {Freq, Pid} -> 
            NewAllocated=lists:keydelete(Freq, 1, Allocated),
            {{[Freq|Free],  NewAllocated}, ok};
        _ -> {{Free, Allocated}, error}
    end.

%% clear function removes any messages in the mailbox and exits when the mailbox is empty
clear() ->
    receive
        Msg ->
            io:format("Discarding: ~w~n", [Msg]),
            clear()
    after 0 -> ok
    end.

%% Randomly  puts the process to sleep to test the timeout code
sleep_random() ->
    RandVal = rand:uniform(),
    case RandVal < 0.4 of
        true -> timer:sleep(120);
        false -> ok
    end.


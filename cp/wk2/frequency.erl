%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0,spawn_client/1]).
-export([init/0,supervisor/0,client/1]).


%% These are the start functions used to create and
%% initialize the server.

start() -> spawn(?MODULE, supervisor, []).

%% Supervisor process for monitoring the server and restarting it in case of an abnormal exit
supervisor() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    receive 
        {'EXIT', _Pid, shutdown} -> 
            io:format("supervisor: Server shutdown"),
            ok;
        {'EXIT', _Pid, Reason} ->
            io:format("supervisor: Server died [~w]~n", [Reason]),
            supervisor()
    end.

init() ->
    process_flag(trap_exit, true),    
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop
loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            Pid ! {reply, ok},
            loop(NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped},
            exit(shutdown);
        {'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
            NewFrequencies = exited(Frequencies, Pid), 
            loop(NewFrequencies)
    end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
        {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
        {reply, Reply} -> Reply
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
        {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    link(Pid),                                               %%% ADDED
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
    unlink(Pid),                                             %%% ADDED
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
    case lists:keysearch(Pid,2,Allocated) of
        {value,{Freq,Pid}} ->
            NewAllocated = lists:keydelete(Freq,1,Allocated),
            {[Freq|Free],NewAllocated}; 
        false ->
            {Free,Allocated} 
    end.


%% Spawn a client with the provided name
spawn_client(Name) -> spawn(?MODULE, client, [Name]).

%% Entrypoint for the client. Registers to listen for exit signals and starts in the disconnected state.
client(Name) -> 
    process_flag(trap_exit, true),    
    disconnected_client(Name).

%% Client in the connected state
%% Sleeps for a random period of time before relinquishing the frequency. If the server dies and restarts 
%% in the middle of the sleep, the server will think the frequency is not allocated eventhough it is still 
%% held by the client until it wakes up. This can only be solved by having persistent state on the server.
connected_client(Name, Freq) ->
    io:format("~s connected. Frequency=~B~n", [Name, Freq]),
    sleep_random(),
    case check_server_status(Name) of
        shutdown -> exit(shutdown);
        dead -> disconnected_client(Name);
        live -> 
            deallocate(Freq),
            disconnected_client(Name)
    end.
    
%% Client in disconnected state
disconnected_client(Name) ->
    io:format("~s disconnected~n", [Name]),
    sleep_random(),
    wait_for_server(Name),
    case allocate() of
        {ok, Freq} -> connected_client(Name, Freq);
        _ -> disconnected_client(Name)
    end.

%% Waits for the server to become available and tries to obtain a frequency
wait_for_server(Name) ->
    case check_server_status(Name) of
        shutdown -> exit(shutdown);
        _ -> 
            case whereis(?MODULE) of
                undefined -> 
                    sleep_random(),
                    wait_for_server(Name);
                _ -> ok
            end
    end.
    
%% Check to see if the server has exited.
check_server_status(Name) ->
    receive
        {'EXIT', _Pid, shutdown} ->
            io:format("~s: Server is shutdown~n", [Name]),
            shutdown;
        {'EXIT', Pid, _Reason} -> 
            io:format("~s: Server is dead~n", [Name]),
            unlink(Pid),
            dead
    after 0 ->
        live
    end.

%% Sleep for a random period of time
sleep_random() ->
    SleepTime = rand:uniform(10) * 2000,
    timer:sleep(SleepTime).




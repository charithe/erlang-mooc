-module(recursion).
-export([fib/1, pieces/1]).

fib(0) ->
    0;
fib(1) ->
    1;
fib(X) when X > 1 ->
    fib(X-1) + fib(X-2).


pieces(0) ->
    1;
pieces(1) ->
    2;
pieces(X) when X > 1 ->
    X + pieces(X-1).

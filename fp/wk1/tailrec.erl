-module(tailrec).
-export([fib/1, perfect/1]).

fib(X) when X >= 0 -> fib(X, 0, 1).

fib(0, A, _B) -> A;
fib(X, A, B) -> fib(X-1, B, A+B).


perfect(X) -> perfect(X, X-1, 0).

perfect(X, 1, A) -> A + 1 == X;
perfect(X, N, A) when X rem N == 0 -> perfect(X, N-1, A+N);
perfect(X, N, A) -> perfect(X, N-1, A).



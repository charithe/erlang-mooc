-module(mylist).
-export([product/1, product_tr/1,  maximum/1, maximum_tr/1, double/1, evens/1, take/2]).

%% Direct recursive
product([]) -> 1;
product([X|Xs]) -> X * product(Xs).

%% Tail recursive
product_tr(X) -> product_tr(X, 1).
product_tr([], P) -> P;
product_tr([X|Xs], P) -> product_tr(Xs, P * X).

%% Direct recursive
maximum([X]) -> X;
maximum([X|Xs]) -> max(X, maximum(Xs)).

%% Tail recursive
maximum_tr(L = [X|_]) -> maximum_tr(L, X).
maximum_tr([X], M) -> max(X, M);
maximum_tr([X|Xs], M) -> maximum_tr(Xs, max(X, M)).

double([]) -> [];
double([X|Xs]) -> [2*X|double(Xs)].

evens([]) -> [];
evens([X|Xs]) when X rem 2 == 0 -> [X|evens(Xs)];
evens([_|Xs]) -> evens(Xs).

take(0,_) -> [];
take(_,[]) -> [];
take(N, [X|Xs]) -> [X|take(N-1, Xs)].

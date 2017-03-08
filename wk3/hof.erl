-module(hof).
-export([doubleAll/1, evens/1, product/1, zip/2, zip_with/3, zip_with_a/3, zip_a/2, compose/1, twice/1, iterate/1]).

doubleAll(Xs) -> lists:map(fun (X) -> 2 * X end, Xs).

evens(Xs) -> lists:filter(fun (X) -> X rem 2 == 0 end, Xs).

product(Xs) -> lists:foldr(fun (X, Acc) -> X * Acc end, 1, Xs).

zip([], _) -> [];
zip(_, []) -> [];
zip([X|Xs], [Y|Ys]) -> [{X, Y} | zip(Xs, Ys)].

zip_with(_, [], _) -> [];
zip_with(_, _, []) -> [];
zip_with(F, [X|Xs], [Y|Ys]) -> [F(X, Y) | zip_with(F, Xs, Ys)].

zip_with_a(F, Xs, Ys) -> lists:map(fun ({X,Y}) -> F(X,Y) end, zip(Xs, Ys)).

zip_a(Xs, Ys) -> zip_with(fun(X, Y) -> {X, Y} end, Xs, Ys).

compose(Fs) -> lists:foldr(fun wrap/2, fun (Y) -> Y end, Fs).

wrap(F, Acc) -> fun(X) -> F(Acc(X)) end.

twice(F) -> compose([F, F]).

iterate(N) when N >= 0 -> fun(F) -> iterate(F, N) end.
iterate(F, 0) -> F;
iterate(F, N) -> F(iterate(F, N-1)).

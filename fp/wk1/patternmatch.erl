-module(patternmatch).
-export([xOr_1/2, xOr_2/2, maxThree/3, howManyEqual/3]).

xOr_1 (X, Y) when X == not Y ->
    true;
xOr_1 (_, _) ->
    false.


xOr_2 (X, Y) ->
    ((not X) and Y) or (X and (not Y)).


maxThree (X, Y, Z) ->
    A = max(X, Y),
    max(A, Z).


howManyEqual(X, X, X) ->
    3;
howManyEqual(X, X, _) ->
    2;
howManyEqual(X, _, X) ->
    2;
howManyEqual(_, X, X) ->
    2;
howManyEqual(_, _, _) ->
    0.

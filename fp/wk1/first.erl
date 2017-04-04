-module(first).
-export([double/1, mult/2, square/1, treble/1]).

mult(X,Y) ->
    X * Y.

double(X) ->
    mult(X,2).

square(X) ->
    mult(X,X).

treble(X) ->
    mult(X, square(X)).

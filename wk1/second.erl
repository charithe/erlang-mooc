-module(second).
-export([hypotenuse/2, perimeter/2, area/2]).

hypotenuse(Opposite, Adjacent) ->
    math:sqrt(first:square(Opposite) + first:square(Adjacent)).

perimeter(Opposite, Adjacent) ->
    Hypotenuse = hypotenuse(Opposite, Adjacent),
    Opposite + Adjacent + Hypotenuse.

area(Opposite, Adjacent) ->
    Hypotenuse = hypotenuse(Opposite, Adjacent),
    (Hypotenuse + Adjacent) / 2.



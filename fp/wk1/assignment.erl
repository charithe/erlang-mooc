-module(assignment).
-export([perimeter/1, area/1, enclose/1, bits/1]).

perimeter({circle, {_X, _Y}, R}) -> 2 * math:pi() * R;

perimeter({rectangle, {_X, _Y}, H, W}) -> 2 * (H + W);

%% A triangle is defined by its' three coordinates. 
perimeter({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) -> 
    A = distance({X1, Y1}, {X2, Y2}),
    B = distance({X1, Y1}, {X3, Y3}),
    C = distance({X2, Y2}, {X3, Y3}),
    A + B + C.

%% Distance formula for calculating the length of line between coordinates (x1, y1) and (x2, y2)
distance({X1, Y1}, {X2, Y2}) -> 
    P = math:pow(X1 - X2, 2),
    Q = math:pow(Y1 - Y2, 2),
    math:sqrt(P + Q).


area({circle, {_X, _Y}, R}) -> math:pi() * math:pow(R, 2);

area({rectangle, {_X, _Y}, H, W}) -> H * W;

area({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) -> 
    A = X1 * (Y2 - Y3),
    B = X2 * (Y3 - Y1),
    C = X3 * (Y1 - Y2),
    abs((A + B + C) / 2).


%% There appears to be many generic solutions to this problem (Eg. Rrotating calipers algorithm).
%% However, since there are only 3 known shapes in this assignment, the solutions for each can be hardcoded
enclose({circle, C, R}) -> {rectangle, C, 2 * R, 2 * R};

enclose({rectangle, C, H, W}) -> {rectangle, C, H, W};

%% Solving for any triangle would require a lot of code so the following assumes that the first two
%% coordinates have the same Y value. In that case, we can define a rectangle centered at C (centroid)
%% with height H (height) and width B (base)
enclose({triangle, {X1, Y}, {X2, Y}, {X3, Y3}}) -> 
    B = distance({X1, Y}, {X2, Y}),
    H = abs(Y3 - Y),
    C = {(X1 + X2 + X3) / 3, (Y + Y + Y3) / 3},
    {rectangle, C, H, B}.


%% This solution is tail recursive
bits(N) when N >= 0 -> bits(N, 0).

bits(0, S) -> S;
bits(N, S) -> 
    Q = N div 2,
    R = N rem 2,
    bits(Q, S + R).


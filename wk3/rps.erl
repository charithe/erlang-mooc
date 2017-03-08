-module(rps).
-export([beat/1, lose/1, result/2, tournament/2]).

beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper.

result(X,X) -> 0;
result(X,Y) -> 
    case beat(X) of
        Y -> -1;
        _ -> 1
    end.

tournament(Left, Right) ->
    Results = lists:zipwith(fun result/2, Left, Right),
    lists:sum(Results).

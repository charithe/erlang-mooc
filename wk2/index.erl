-module(index).
-export([do_index/1, show_file_contents/1]).

%% Main entrypoint 
%% Reads the contents of the file using the provided functions, builds up an associative array 
%% In the Erlang shell, execute rp(index:do_index("path_to_file")).
%% of words to their locations, compresses the locations to ranges and returns a list sorted 
%% in alphabetical order
do_index(Name) ->
    Lines = get_file_contents(Name),
    Index = build_index(Lines),
    CompressedIndex = maps:map(fun(_Word, Locations) -> build_ranges(lists:sort(Locations)) end, Index),
    lists:sort(fun({A, _}, {B, _}) -> A =< B end, maps:to_list(CompressedIndex)).

%% Provided Function
% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

%% Provided Function
% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

%% Provided Function
% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    
     

%% Builds up the index by processing the lines returned from the file reading function
%% Each line is lowercased and split on non-word characters to obtain a list of words.
%% These words are then added to the map along with their location.
build_index(Lines) -> build_index(Lines, 1, maps:new()).

build_index([], _, Index) -> Index;
build_index([H|T], N, Index) -> 
    Words = split_to_words(H),
    UpdatedIndex = add_words_to_index(Words, N, Index),
    build_index(T, N+1, UpdatedIndex).

%% Lowercase a line and split into a list of words, removing any non-word characters such as punctuation
split_to_words(Line) -> re:split(string:to_lower(Line), "\\W", [{return, list}, trim, notempty]).

%% Add a list of words and their location to the map containing the index. 
add_words_to_index([], _, Index) -> Index;
add_words_to_index([[]|T], N, Index) -> add_words_to_index(T, N, Index);
add_words_to_index([H|T], N, Index) ->
    case maps:find(H, Index) of
        {ok, Lines} -> add_words_to_index(T, N, maps:put(H, [N|Lines], Index));
        _ -> add_words_to_index(T, N, maps:put(H, [N], Index))
    end.

%% Convert a sorted list of integers into a list of tuples containing ranges
build_ranges([]) -> [];
build_ranges([H|T]) -> build_ranges(T, H, H).

build_ranges([], RangeStart, RangeEnd) -> [{RangeStart, RangeEnd}];
build_ranges([H|T], RangeStart, RangeEnd) when H == RangeEnd + 1 -> build_ranges(T, RangeStart, H);
build_ranges([H|T], RangeStart, RangeEnd) -> [{RangeStart, RangeEnd}|build_ranges(T, H, H)]. 

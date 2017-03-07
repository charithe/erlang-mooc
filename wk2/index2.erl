-module(index2).
-export([get_file_contents/1, show_file_contents/1, index_file/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
  {ok, File} = file:open(Name, [read]),
  Rev = get_all_lines(File, []),
  lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File, Partial) ->
  case io:get_line(File, "") of
    eof -> file:close(File),
      Partial;
    Line -> {Strip, _} = lists:split(length(Line) - 1, Line),
      get_all_lines(File, [Strip | Partial])
  end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L | Ls]) ->
  io:format("~s~n", [L]),
  show_file_contents(Ls);
show_file_contents([]) ->
  ok.


%%%
%%% Reverse a list
%%%
reverse(List) when is_list(List) ->
  reverse(List, []).
reverse([], Acc) -> Acc;
reverse([X | XS], Acc) -> reverse(XS, [X | Acc]).


%%%
%%% Apply the Fun to transform all element of the list into a new list of transformed item.
%%%
map(Fun, XS) ->
  map(Fun, XS, []).

map(_Fn, [], Acc) -> reverse(Acc);
map(Fun, [X | XS], Acc) -> map(Fun, XS, [Fun(X) | Acc]).


%%%
%%% Return a filtered list that contains only the item that match the filter.
%%%
filter(Fun, XS) ->
  filter(Fun, XS, []).

filter(_Fn, [], Acc) -> reverse(Acc);
filter(Fun, [X | XS], Acc) ->
  case Fun(X) of
    true ->
      filter(Fun, XS, [X | Acc]);
    _ ->
      filter(Fun, XS, Acc)
  end.


%%%
%%% Split a Text into Words
%%%
to_words(Text) ->
  Separators = [$\s, $., $,, $;, $", $`, $!, $?, $-, $", $‘],
  to_words(Separators, [], Text, []).

to_words(_, [], [], Words) ->
  reverse(Words);
to_words(Separators, AccText, [], Words) ->
  to_words(Separators, [], [], [reverse(AccText) | Words]);
to_words(Separators, AccText, [Char | Text], Words) ->
  case member(Separators, Char) of
    true ->
      Word = reverse(AccText),
      case length(Word) of
        0 -> % Empty word case: don't keep it
          to_words(Separators, [], Text, Words);
        _ ->
          to_words(Separators, [], Text, [Word | Words])
      end;
    _ ->
      to_words(Separators, [Char | AccText], Text, Words)
  end.

%%%
%%%
%%%
to_lower_case(Text) when is_list(Text) ->
  map(fun to_lower_case/1, Text);
to_lower_case(Char) when Char >= $A andalso Char =< $Z ->
  Char - ($A - $a);
to_lower_case(Char) -> Char.


%%%
%%% Discard duplicate value e.g. Set behavior
%%%
unique(LS) ->
  unique(LS, []).
unique([], Acc) -> Acc;
unique([X | XS], Acc) ->
  unique(remove_all(XS, X), [X | Acc]).

%%%
%%% Remove all X value occurrences from the XS list
%%%
remove_all(XS, X) ->
  remove_all(XS, X, []).
remove_all([], _X, Acc) -> reverse(Acc);
remove_all([X | XS], X, Acc) -> remove_all(XS, X, Acc);
remove_all([Y | XS], X, Acc) -> remove_all(XS, X, [Y | Acc]).

%%%
%%% Indicates whether or not a value is member of the provided List.
%%%
member([], _) -> false;
member([X | _S], X) -> true;
member([_ | XS], X) -> member(XS, X).

%%%
%%% Indicates whether or not a word is accepted according to the provided rule.
%%%
-type rule() :: fun((string()) -> boolean()).
-spec accepts_word([rule()], string()) -> boolean().
accepts_word([], _Word) -> true;
accepts_word([Rule | Rules], Word) ->
  case Rule(Word) of
    true -> accepts_word(Rules, Word);
    _ -> false
  end.

%%%
%%% Rule: The word is not a known stop word
%%%
-define(STOP_WORDS, ["a", "about", "above", "above", "across", "after", "afterwards", "again", "against", "all", "almost",
  "alone", "along", "already", "also", "although", "always", "am", "among", "amongst", "amoungst", "amount", "an", "and",
  "another", "any", "anyhow", "anyone", "anything", "anyway", "anywhere", "are", "around", "as", "at", "back", "be",
  "became", "because", "become", "becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below",
  "beside", "besides", "between", "beyond", "bill", "both", "bottom", "but", "by", "call", "can", "cannot", "cant",
  "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each",
  "eg", "eight", "either", "eleven", "else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone",
  "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former",
  "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt",
  "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself",
  "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself",
  "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill",
  "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never",
  "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off",
  "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out",
  "over", "own", "part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming",
  "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some",
  "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten",
  "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore",
  "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through",
  "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un",
  "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence",
  "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while",
  "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you",
  "your", "yours", "yourself", "yourselves", "the"]).
stop_word_rule(Word) ->
  case member(?STOP_WORDS, Word) of
    true -> false;
    _ -> true
  end.

%%%
%%% Rule: The word length must be greater than 3.
%%%
greater_than_three_rule(Word) ->
  case length(Word) of
    N when N > 3 -> true;
    _ -> false
  end.

%%%
%%% Transform a word to its stemmed representation.
%%% https://en.wikipedia.org/wiki/Stemming
%%%
to_stem(Word) ->
  Lowered = to_lower_case(Word),
  reverse(suffix_stripping(reverse(Lowered))).

% suffix-stripping algorithm
suffix_stripping([$d, $e, $i | Remaining]) -> [$y | Remaining]; % replace >>ied<< by >>y<<
suffix_stripping([$d, $e | Remaining]) -> Remaining; % discard >>ed<<
suffix_stripping([$g, $n, $i | Remaining]) -> Remaining; % discard >>ing<<
suffix_stripping([$y, $l | Remaining]) -> Remaining; % discard >>ly<<
suffix_stripping([$s, $' | Remaining]) -> Remaining; % discard >>'s<<
suffix_stripping([$t, $', $n | Remaining]) -> Remaining; % discard >>n't<<
suffix_stripping(Remaining) -> Remaining.


lookup_and_remove_index_for_word(Word, Index) ->
  lookup_and_remove_index_for_word(Word, Index, []).

lookup_and_remove_index_for_word(_Wrd, [], AlreadyTraversed) ->
  {AlreadyTraversed, []};
lookup_and_remove_index_for_word(Word, [{Word, Refs} | Index], AlreadyTraversed) ->
  {Index ++ AlreadyTraversed, Refs};
lookup_and_remove_index_for_word(Word, [WordRef | Index], AlreadyTraversed) ->
  lookup_and_remove_index_for_word(Word, Index, [WordRef | AlreadyTraversed]).

%%%
%%% Merge range of line reference, probably the trickiest part of the code
%%% [{1,1}       ], 3  ==>  {1,1}, {3,3}
%%% [{1,1}       ], 2  ==>  {1,2}
%%% [{1,1}, {3,4}], 2  ==>  {1,4}
%%%
merge_refs(Refs, LineNb) ->
  Inserted = reverse(insert([], LineNb, Refs)),
  reverse(merge_range([], Inserted)).

insert(Acc, LineNb, []) -> reverse([{LineNb, LineNb} | Acc]);
insert(Acc, LineNb, [C = {Lmin, Lmax} | Others]) when Lmin =< LineNb andalso LineNb =< Lmax ->
  reverse(Others) ++ [C | Acc];
insert(Acc, LineNb, [C = {Lmin, _Lmax} | Others]) when Lmin < LineNb ->
  reverse(Others) ++ [{LineNb, LineNb}, C | Acc];
insert(Acc, LineNb, [C = {_Lmin, Lmax} | Others]) when Lmax < LineNb ->
  reverse(Others) ++ [C, {LineNb, LineNb} | Acc];
insert(Acc, LineNb, [C | Others]) ->
  insert([C | Acc], LineNb, Others).


merge_range(Acc, []) -> Acc;
merge_range(Acc, [{Min1, Max1}, {Min2, Max2} | Others]) when Max1 + 1 == Min2 ->
  merge_range(Acc, [{Min1, Max2} | Others]);
merge_range(Acc, [C | Others]) ->
  merge_range([C | Acc], Others).

%%%
%%%
%%%
update_index(_LinNb, [], Index) -> Index;
update_index(LineNb, [Word | Words], Index) ->
  {IndexWithoutWord, Refs} = lookup_and_remove_index_for_word(Word, Index),
  NewRefs = merge_refs(Refs, LineNb),
  NewIndex = [{Word, NewRefs} | IndexWithoutWord],
  update_index(LineNb, Words, NewIndex).

%%%
%%% Fill the Index line by line.
%%%
index(_LinNb, [], Index) -> Index;
index(LineNb, [Line | Lines], Index) ->
  Words = to_words(Line),
  Stems = map(fun to_stem/1, Words),
  Filtered = filter(fun(Word) -> accepts_word([
    fun greater_than_three_rule/1,
    fun stop_word_rule/1], Word) end,
    Stems),
  NewIndex = update_index(LineNb, Filtered, Index),
  index(LineNb + 1, Lines, NewIndex).

%%%
%%% Basic entry point that read the content of the file, and index all the lines one by one.
%%% Index is then dumped into the standard output.
%%%
index_file(FileName) ->
  Lines = get_file_contents(FileName),
  Index = index(1, Lines, []),
  io:format("Index: ~p~n", [Index]),
  Index.

% -------------------------------------
% TEST
% -------------------------------------
%
% eunit:test(index).
%
-include_lib("eunit/include/eunit.hrl").


reverse_test() ->
  ?assertEqual([], reverse([])),
  ?assertEqual([3, 4, 1], reverse([1, 4, 3])).

map_test() ->
  ?assertEqual([], map(fun(X) -> 2 * X end, [])),
  ?assertEqual([2, 4, 8], map(fun(X) -> 2 * X end, [1, 2, 4])).

filter_test() ->
  ?assertEqual([], filter(fun(X) -> X > 2 end, [])),
  ?assertEqual([3, 4], filter(fun(X) -> X > 2 end, [1, 3, 2, 4, 1])).

to_words_test() ->
  Words = to_words("I don't mean to say that I know"),
  ?assertEqual(["I", "don't", "mean", "to", "say", "that", "I", "know"], Words).

to_words__special_chars__test() ->
  Words = to_words("     `Here's Martha, mother.' said a girl, appearing as she"),
  ?assertEqual(["Here's", "Martha", "mother", "'", "said", "a", "girl", "appearing", "as", "she"], Words).

unique_test() ->
  Words = unique(["I", "don't", "mean", "to", "say", "that", "I", "know"]),
  ?assertEqual(["I", "don't", "know", "mean", "say", "that", "to"], lists:sort(Words)).

remove_all_test() ->
  ?assertEqual([1, 2], remove_all([1, 2], 3)),
  ?assertEqual([1, 2], remove_all([1, 2, 3, 3], 3)),
  ?assertEqual([], remove_all([3], 3)).

member_test() ->
  ?assertEqual(true, member([a, b, c], a)),
  ?assertEqual(true, member([a, b, a, c], a)),
  ?assertEqual(false, member([b, c], a)),
  ?assertEqual(false, member([], a)).

to_lower_case_test() ->
  ?assertEqual("", to_lower_case("")),
  ?assertEqual("aze", to_lower_case("AZE")),
  ?assertEqual("aze-12-bgh", to_lower_case("AzE-12-BGh")).

stop_word_rule_test() ->
  ?assertEqual(false, stop_word_rule("been")),
  ?assertEqual(false, stop_word_rule("none")),
  ?assertEqual(false, stop_word_rule("since")),
  ?assertEqual(false, stop_word_rule("least")),
  ?assertEqual(true, stop_word_rule("noop")).

greater_than_three_rule_test() ->
  ?assertEqual(false, greater_than_three_rule("")),
  ?assertEqual(false, greater_than_three_rule("a")),
  ?assertEqual(false, greater_than_three_rule("ab")),
  ?assertEqual(false, greater_than_three_rule("abc")),
  ?assertEqual(true, greater_than_three_rule("abcd")).

lookup_and_remove_index_for_word__present_case__test() ->
  Index = [
    {"wish", [{5, 5}]},
    {"houses", [{4, 4}]},
    {"season", [4]},
    {"ghost", [{1, 2}]}],
  {NewWishIndex, WishRef} = lookup_and_remove_index_for_word("wish", Index),
  ?assertEqual([{5, 5}], WishRef),
  ?assertEqual(
    lists:sort([
      {"houses", [{4, 4}]},
      {"season", [4]},
      {"ghost", [{1, 2}]}]),
    lists:sort(NewWishIndex)).

lookup_and_remove_index_for_word__missingt_case__test() ->
  Index = [
    {"wish", [{5, 5}]},
    {"houses", [{4, 4}]},
    {"season", [4]},
    {"ghost", [{1, 2}]}],
  {NewIndex, MissingRef} = lookup_and_remove_index_for_word("missing_word", Index),
  ?assertEqual([], MissingRef),
  ?assertEqual(lists:sort(Index), lists:sort(NewIndex)).

merge_refs__test() ->
  ?assertEqual([{1, 1}], merge_refs([], 1)),
  ?assertEqual([{1, 4}], merge_refs([{1, 4}], 2)),
  ?assertEqual([{1, 1}, {3, 3}], merge_refs([{1, 1}], 3)),
  ?assertEqual([{1, 1}, {3, 3}], merge_refs([{3, 3}], 1)),
  ?assertEqual([{1, 2}], merge_refs([{1, 1}], 2)),
  ?assertEqual([{1, 4}], merge_refs([{1, 1}, {3, 4}], 2)),
  ?assertEqual([{15,15},{25,25},{22,22}], merge_refs([{15, 15}, {22, 22}], 25)).

usecase_test() ->
  Lines = ["I have endeavoured in this Ghostly little book,",
    "to raise the Ghost of an Idea, which shall not put my",
    "readers out of humour with themselves, with each other,",
    "with the season, or with me.  May it haunt their houses",
    "pleasantly, and no one wish to lay it."
  ],
  Index = index(1, Lines, []),
  %%error_logger:info_msg("Index ~p", [Index]),
  ?assertEqual(true, member(Index, {"ghost", [{1, 2}]})),
  ?assertEqual(true, member(Index, {"pleasant", [{5, 5}]})).


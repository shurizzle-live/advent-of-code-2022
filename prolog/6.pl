:- use_module(library(pio)).

% Definite clause grammar {{{
blank --> [C], { nonvar(C), code_type(C, space) }.
blanks --> blank, !, blanks.
blanks --> [].

nonblank(C) --> [C], { nonvar(C), \+ code_type(C, space) }.
nonblanks([C|Cs]) --> nonblank(C), !, nonblanks(Cs).
nonblanks([]) --> [].

parse(Cs) --> blanks, nonblanks(Cs), blanks.
% }}}

slices(Data, Len, L0:Slice) :-
  length(Data, L),
  L >= Len,
  MaxPre is L - Len,
  between(0, MaxPre, PreLen),
  length(Pre, PreLen),
  length(Slice, Len),
  append(Pre, Slice, P),
  append(P, _, Data),
  length(P, L0).

unique(List) :-
  sort(List, List0),
  length(List, L),
  length(List0, L).

solve1(Data, Res) :-
  slices(Data, 4, Res:Slice),
  unique(Slice), !.
solve2(Data, Res) :-
  slices(Data, 14, Res:Slice),
  unique(Slice), !.

input(Data) :-
  phrase_from_file(parse(Data), "inputs/6.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 1: ~w~n", [Two]).

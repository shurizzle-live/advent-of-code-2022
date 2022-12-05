:- use_module(library(pio)).
:- use_module(library(yall)).

% Definite clause grammar {{{
blank --> [C], { nonvar(C), code_type(C, space) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> [C], { code_type(C, newline) }.

lines([O:M|Xs]) --> blanks, line(O:M), !, lines0(Xs), blanks.

lines0([O:M|Xs]) --> nl, line(O:M), !, lines0(Xs).
lines0([]) --> [].

line(O:M) --> opponent(O), " ", me(M).

opponent(rock) --> "A".
opponent(paper) --> "B".
opponent(scissors) --> "C".

me(rock) --> "X".
me(paper) --> "Y".
me(scissors) --> "Z".
% }}}

% test {{{
test_data("A Y
B X
C Z").

data(Es) :-
  test_input(I),
  string_codes(I, Is),
  phrase(lines(Es), Is).

test_input(Data) :-
  test_data(I),
  string_codes(I, Is),
  phrase(lines(Data), Is).

test_solution1(Res) :-
  test_input(Data),
  solve1(Data, Res).

test_solution2(Res) :-
  test_input(Data),
  solve2(Data, Res).

test :-
  test_input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 1: ~w~n", [Two]).
% }}}

%! round_outcome(?Opponent, ?Me, ?Result, +Value) is det
%! round_outcome(+Opponent, +Me, ?Result, ?Value) is det
%! round_outcome(+Opponent, ?Me, +Result, ?Value) is det
%! round_outcome(?Opponent, +Me, +Result, ?Value) is det
%! round_outcome(?Opponent, ?Me, ?Result, ?Value) is nondet
round_outcome(O, M, R, V) :-
  nonvar(V), !,
  once(round_outcome_(O, M, R, V)).
round_outcome(O, M, R, V) :-
  ( nonvar(O), (nonvar(M); nonvar(R))
  ; nonvar(R), nonvar(M) ), !,
  once(round_outcome_(O, M, R, V)).
round_outcome(O, M, R, V) :-
  round_outcome_(O, M, R, V).

round_outcome_(rock, rock, draw, 4).
round_outcome_(rock, paper, win, 8).
round_outcome_(rock, scissors, loss, 3).
round_outcome_(paper, rock, loss, 1).
round_outcome_(paper, paper, draw, 5).
round_outcome_(paper, scissors, win, 9).
round_outcome_(scissors, rock, win, 7).
round_outcome_(scissors, paper, loss, 2).
round_outcome_(scissors, scissors, draw, 6).

%! remap_me(+Shape, ?Result) is det
%! remap_me(?Shape, +Result) is det
%! remap_me(?Shape, ?Result) is nondet
remap_me(rock, loss).
remap_me(paper, draw).
remap_me(scissors, win).

solve1(Data, Res) :-
  foldl([O:M, Acc0, Acc]>>(
    round_outcome(O, M, _, V),
    Acc is Acc0 + V
  ), Data, 0, Res).
solve2(Data, Res) :-
  foldl([O:Res0, Acc0, Acc]>>(
    remap_me(Res0, Res),
    round_outcome(O, _, Res, V),
    Acc is Acc0 + V
  ), Data, 0, Res).

input(Data) :-
  phrase_from_file(lines(Data), "inputs/2.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 1: ~w~n", [Two]).

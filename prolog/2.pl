:- use_module(library(pio)).
:- use_module(library(yall)).

:- op(700, xfx, beats).

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

cut_if(MustCut, Goal) :-
  (
      MustCut
  ->  once(Goal)
  ;   Goal
  ).

cut_unless(MustCut, Goal) :-
  cut_if(\+ MustCut, Goal).

shape(rock).
shape(paper).
shape(scissors).

result(draw).
result(win).
result(loss).

beats(X, Y) :-
  cut_unless((var(X), var(Y)), beats_(X, Y)).

beats_(rock, scissors).
beats_(scissors, paper).
beats_(paper, rock).

shape_value(X, Y) :-
  cut_unless((var(X), var(Y)), shape_value_(X, Y)).
shape_value_(rock, 1).
shape_value_(paper, 2).
shape_value_(scissors, 3).

round_result(X, Y, Z) :-
  nonvar(Z), !, result(Z),
  (
      Z == draw
  ->  Y = X, shape(X)
  ;   Z == win
  ->  Y beats X
  ;   X beats Y
  ).
round_result(X, Y, Z) :-
  shape(X), shape(Y),
  (
      X == Y
  ->  Z = draw
  ;   X beats Y
  ->  Z = loss
  ;   Z = win
  ).

result_value(X, Y) :-
  cut_unless((var(X), var(Y)), result_value_(X, Y)).

result_value_(loss, 0).
result_value_(draw, 3).
result_value_(win,  6).

round_outcome(X, Y, Res, V) :-
  round_result(X, Y, Res),
  result_value(Res, V0),
  shape_value(Y, V1),
  V is V0 + V1.

remap_me(X, Y) :-
  cut_unless((var(X), var(Y)), remap_me_(X, Y)).
remap_me_(rock, loss).
remap_me_(paper, draw).
remap_me_(scissors, win).

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

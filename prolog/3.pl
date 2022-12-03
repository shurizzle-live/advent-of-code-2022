:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(yall)).

% test {{{
test_data("vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw").

data(Es) :-
  test_input(I),
  string_codes(I, Is),
  phrase(parse(Es), Is).

test_input(Data) :-
  test_data(I),
  string_codes(I, Is),
  phrase(parse(Data), Is).

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

% Definite clause grammar {{{
blank --> [C], { nonvar(C), code_type(C, space) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> [C], { code_type(C, newline) }.

parse([R|Rs]) --> blanks, rucksack(R), parse0(Rs), blanks.
parse0([R|Rs]) --> nl, rucksack(R), !, parse0(Rs).
parse0([]) --> [].

rucksack([C|Cs]) --> char(C), chars(Cs).

char(C) --> [C0], { between(0'a, 0'z, C0), C is C0 - 0'a + 1 }, !.
char(C) --> [C0], { between(0'A, 0'Z, C0), C is C0 - 0'A + 27 }.

chars([C|Cs]) --> char(C), !, chars(Cs).
chars([]) --> [].
% }}}

invalid_element(Xs, E) :-
  length(Xs, L0),
  L is L0 / 2,
  length(A, L),
  length(B, L),
  append(A, B, Xs),
  member(E, A),
  member(E, B), !.

chunk3([A,B,C|_], [A,B,C]).
chunk3([_,_,_|T], Res) :- length(T, L), L > 0, chunk3(T, Res).

solve1(Data, Res) :-
  foldl([Xs, Acc0, Acc]>>(
    invalid_element(Xs, E),
    Acc is Acc0 + E
  ), Data, 0, Res).
solve2(Data, Res) :-
  aggregate_all(sum(X), (
    chunk3(Data, [A, B, C]),
    once((
      member(X, A),
      member(X, B),
      member(X, C)
    ))
  ), Res).

input(Data) :-
  phrase_from_file(parse(Data), "inputs/3.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 1: ~w~n", [Two]).

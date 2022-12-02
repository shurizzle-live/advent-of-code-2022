:- use_module(library(pio)).
:- use_module(library(ordsets)).

% Definite clause grammar {{{
blank --> [C], { nonvar(C), code_type(C, space) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> [C], { code_type(C, newline) }.

elves(Es) -->
  blanks, elf(E), elves0(Es0), blanks,
  { ord_add_element(Es0, E, Es) }.

elves0(Es) -->
  nl, nl, elf(E), elves0(Es0), !,
  { ord_add_element(Es0, E, Es) }.
elves0([]) --> [].

elf(I) --> integer(I0), elf0(I1), { I is I0 + I1 }.

elf0(I) --> nl, integer(I0), elf0(I1), !, { I is I0 + I1 }.
elf0(0) --> [].

integer(I) -->
  digit(C), integer0(Cs),
  { number_codes(I, [C|Cs]) }.
integer0([C|Cs]) --> digit(C), integer0(Cs), !.
integer0([]) --> [].

digit(C) --> [C], { nonvar(C), code_type(C, digit) }.
% }}}

% test {{{
test_data("1000
2000
3000

4000

5000
6000

7000
8000
9000

10000").

data(Es) :-
  test_input(I),
  string_codes(I, Is),
  phrase(elves(Es), Is).

test_input(Data) :-
  test_data(I),
  string_codes(I, Is),
  phrase(elves(Data), Is).

test_solution1(Res) :-
  test_input(Elves),
  solve1(Elves, Res).

test_solution2(Res) :-
  test_input(Elves),
  solve2(Elves, Res).

test :-
  test_input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 1: ~w~n", [Two]).
% }}}

solve1(Data, Res) :- last(Data, Res).

solve2(Data, Res) :-
  length(Max, 3),
  once(append(_, Max, Data)),
  sum_list(Max, Res).

input(Data) :-
  phrase_from_file(elves(Data), "inputs/1.txt").

solution1(Res) :- input(Elves), solve1(Elves, Res).

solution2(Res) :- input(Elves), solve2(Elves, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 1: ~w~n", [Two]).

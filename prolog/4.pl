:- use_module(library(pio)).

% test {{{
test_data("2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8").

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

parse([L|Ls]) --> blanks, line(L), !, parse_(Ls), blanks.

parse_([L|Ls]) --> nl, line(L), !, parse_(Ls).
parse_([]) --> [].

line((R1,R2)) --> range(R1), ",", range(R2).

range(Start:End) --> integer(Start), "-", integer(End).

integer(I) -->
  digit(C), integer0(Cs),
  { number_codes(I, [C|Cs]) }.
integer0([C|Cs]) --> digit(C), integer0(Cs), !.
integer0([]) --> [].

digit(C) --> [C], { nonvar(C), code_type(C, digit) }.
% }}}

:- op(900, xfx, contains).

contains(AStart:AEnd, BStart:BEnd) :-
  AStart =< BStart,
  AEnd >= BEnd.

:- op(900, xfx, overlaps).

overlaps(AStart:AEnd, BStart:BEnd) :-
  AStart =< BEnd,
  BStart =< AEnd.

solve1(Data, Res) :-
  aggregate_all(
    count,
    (
      member((A,B), Data),
      once((A contains B; B contains A))
    ),
    Res
  ).
solve2(Data, Res) :-
  aggregate_all(
    count,
    (
      member((A,B), Data),
      A overlaps B
    ),
    Res
  ).

input(Data) :-
  phrase_from_file(parse(Data), "inputs/4.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 1: ~w~n", [Two]).


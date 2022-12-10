:- use_module(library(pio)).

test_input_("30373
25512
65332
33549
35390").

test_input(I) :-
  test_input_(I0),
  string_codes(I0, I).

% Definite clause grammar {{{
blank --> [C], { nonvar(C), code_type(C, space) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

digit(C) -->
  [C0],
  { nonvar(C0),
    code_type(C0, digit),
    C is C0 - 0'0 }.

line([D|Ds]) --> digit(D), line_(Ds).
line_([D|Ds]) --> digit(D), !, line_(Ds).
line_([]) --> [].

parse([L|Ls]) --> blanks, line(L), parse_(Ls), blanks.
parse_([L|Ls]) --> nl, line(L), !, parse_(Ls).
parse_([]) --> [].
% }}}

split_at(Xs, Pos, A, B) :-
  length(Xs, L),
  L1 is Pos-1,
  L2 is L-Pos,
  length(A, L1),
  length(B, L2),
  once(append([A,_,B], Xs)).

solve1(Data, Res) :-
  nth0(0, Data, L0),
  length(L0, LH),
  length(Data, LV),

  aggregate_all(
    count,
    (
      between(1, LV, Y),
      between(1, LH, X),

      nth1(Y, Data, T0),
      nth1(X, T0, Value),

      nth1(Y, Data, Horiz),
      split_at(Horiz, X, L, R),

      maplist(nth1(X), Data, Vert),
      split_at(Vert, Y, U, D),

      once((
        maplist(>(Value), L)
      ; maplist(>(Value), R)
      ; maplist(>(Value), U)
      ; maplist(>(Value), D)
      ))
    ),
    Res
  ).

scenic_score_([], _, Res, Res) :- !.
scenic_score_([X|Xs], V, Acc0, Res) :-
  (   V > X
  ->  Acc is Acc0 + 1,
      scenic_score_(Xs, V, Acc, Res)
  ;   Res is Acc0 + 1
  ).
scenic_score_(Xs, V, Res) :-
  scenic_score_(Xs, V, 0, Res).

scenic_score([], _, Res, Res) :- !.
scenic_score([Xs|Xss], V, Acc0, Res) :-
  scenic_score_(Xs, V, Score),
  Acc is Acc0 * Score,
  scenic_score(Xss, V, Acc, Res).

scenic_score(Xss, V, Res) :-
  scenic_score(Xss, V, 1, Res).

solve2(Data, Res) :-
  nth0(0, Data, L0),
  length(L0, LH),
  length(Data, LV),

  aggregate_all(
    max(Score),
    (
      between(1, LV, Y),
      between(1, LH, X),

      nth1(Y, Data, T0),
      nth1(X, T0, Value),

      nth1(Y, Data, Horiz),
      split_at(Horiz, X, L1, R),
      reverse(L1, L),

      maplist(nth1(X), Data, Vert),
      split_at(Vert, Y, U0, D),
      reverse(U0, U),

      scenic_score([L, R, U, D], Value, Score)
    ),
    Res
  ).

input(Data) :-
  phrase_from_file(parse(Data), "inputs/8.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 2: ~w~n", [Two]).

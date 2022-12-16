:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(util).

% Definite clause grammar {{{
blank --> [C], { nonvar(C), code_type(C, space) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

integer(I) -->
  digit(C), integer0(Cs),
  { number_codes(I, [C|Cs]) }.
integer0([C|Cs]) --> digit(C), !, integer0(Cs).
integer0([]) --> [].

digit(C) --> [C], { nonvar(C), code_type(C, digit) }.

sign_integer(I) --> "-", !, integer(I0), { I is -I0 }.
sign_integer(I) --> "+", !, integer(I).
sign_integer(I) --> integer(I).

line(((#(X1), #(Y1)), (#(X2), #(Y2)))) -->
  "Sensor at x=", sign_integer(X1),
  ", y=", sign_integer(Y1),
  ": closest beacon is at x=", sign_integer(X2),
  ", y=", sign_integer(Y2).

parse([L|Ls]) --> blanks, line(L), parse_(Ls), blanks.
parse_([L|Ls]) --> nl, line(L), !, parse_(Ls).
parse_([]) --> [].
% }}}

manhattan((#(X1), #(Y1)), (#(X2), #(Y2)), #(D)) :-
  D #= abs(X1 - X2) + abs(Y1 - Y2).

make_range([I], D) :- !, D in I..I.
make_range([I1, I2], D) :- D in I1..I2.

domain_joiner(D1, D2, D) :-
  fd_dom(D1, A),
  fd_dom(D2, B),
  D in A\/B.

y_bounds(((#(X1), #(Y1)), (#(X2), #(Y2))), #(Y), Bounds) :-
  manhattan((#(X1), #(Y1)), (#(X2), #(Y2)), #(Dist)),
  findall(X, (
    manhattan((#(X1), #(Y1)), (#(X), #(Y)), #(Dist)),
    indomain(X)
  ), Bounds0),
  make_range(_-Bounds0, _-Bounds).

solve1(Data, Res) :-
  Y = 2000000,
  % Y = 10,
  findall(Bounds, (
    member(L, Data),
    y_bounds(L, #(Y), Bounds)
  ), Bounds0),
  foldl1(domain_joiner, Bounds0, Bounds1),
  fd_size(Bounds1, Res0),
  Res is Res0 - 1.

overlaps((AStart, AEnd), (BStart, BEnd)) :-
  AStart =< BEnd,
  BStart =< AEnd.

map_range(K-[I], K-I) :- !.
map_range(K-[I1, I2], K-Range) :-
  domain_joiner(I1, I2, Range).

contraint_bounds(Max, I1..I2, Range) :-
  overlaps((0, Max), (I1, I2)),
  A is max(0, I1),
  B is min(Max, I2),
  Range = A..B.

fold_range(_, [], Res, Res) :- !.
fold_range(Max, [Bound|R], Acc0, Res) :-
  (   map_range(Bound, B)
  ->  Acc = [B|Acc0]
  ;   Acc = Acc0
  ),
  fold_range(Max, R, Acc, Res).

foldy(Min, Y, _, _, _, Res, Res) :- Y < Min, !.
foldy(Min, Y0, Max, P, #(Dist), Acc0, Res) :-
  findall(X, (
    manhattan(P, (#(X), #(Y0)), #(Dist)),
    indomain(X)
  ), Xs),
  (   [A, B] = Xs
  ->  Bounds0 = A..B
  ;   [A] = Xs,
      Bounds0 = A..A
  ),
  (   contraint_bounds(Max, Bounds0, Bounds)
  ->  Acc = [Y0-Bounds|Acc0]
  ;   Acc = Acc0
  ),
  Y is Y0 - 1,
  foldy(Min, Y, Max, P, #(Dist), Acc, Res).

max_bounds(((#(X1), #(Y1)), (#(X2), #(Y2))), #(Max), Bounds) :-
  manhattan((#(X1), #(Y1)), (#(X2), #(Y2)), #(Dist)),

  YMin0 is Y1 - Dist,
  YMax0 is Y1 + Dist,
  contraint_bounds(Max, YMin0..YMax0, YMin..YMax),

  foldy(YMin, YMax, Max, (#(X1), #(Y1)), #(Dist), [], Bounds0),
  sort(Bounds0, Bounds1),
  group_pairs_by_key(Bounds1, Bounds2),
  fold_range(Max, Bounds2, [], Bounds).

merge_pair(K-[D], K-D) :- !.
merge_pair(K-[D1, D2], K-D) :-
  D0 in D1\/D2,
  fd_dom(D0, D).

merge_pairs(P1, P2, Pairs) :-
  append(P1, P2, P0),
  sort(P0, P3),
  group_pairs_by_key(P3, P4),
  maplist(merge_pair, P4, Pairs).

filter_size(Size, _-D) :-
  X in D,
  fd_size(X, S),
  Size == S.

fold_bounds([], _, Res, Res) :- !.
fold_bounds([L|R], Max, Acc0, Res) :-
  format("~w~n", [L]),
  (   max_bounds(L, #(Max), Bounds)
  ->  merge_pairs(Bounds, Acc0, Acc)
  ;   Acc = Acc0
  ),
  fold_bounds(R, Max, Acc, Res).

solve2(Data, Res) :-
  % Max = 20,
  Max = 4 000 000,
  fold_bounds(Data, Max, [], Bounds1),
  Max1 is Max + 1,
  exclude(filter_size(Max1), Bounds1, [Y-X0]),
  X1 in X0,
  findall(X1, indomain(X1), Xs),
  X in 0..Max,
  maplist(#\=(X), Xs),
  Res is X * 4 000 000 + Y.

input(Data) :-
  phrase_from_file(parse(Data), "inputs/15.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 2:~n~s~n", [Two]).

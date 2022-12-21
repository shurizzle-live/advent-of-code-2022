:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).

% Definite clause grammar {{{
blank --> [C], { nonvar(C), code_type(C, space) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

digit(C) --> [C], { nonvar(C), code_type(C, digit) }.

integer(I) -->
  digit(C), integer0(Cs),
  { number_codes(I, [C|Cs]) }.
integer0([C|Cs]) --> digit(C), !, integer0(Cs).
integer0([]) --> [].

name(N) -->
  [A, B, C, D],
  { code_type(A, lower),
    code_type(B, lower),
    code_type(C, lower),
    code_type(D, lower),
    atom_codes(N, [A, B, C, D]) }.

operator(+) --> "+", !.
operator(-) --> "-", !.
operator(*) --> "*", !.
operator(//) --> "/", !.

line(K-V) -->
  name(K), ": ",
  name(O1), " ", operator(O), " ", name(O2), !,
  { V =.. [O, O1, O2] }.
line(K-V) -->
  name(K), ": ", integer(V).

parse([L|Ls]) --> blanks, line(L), parse_(Ls), blanks.
parse_([L|Ls]) --> nl, line(L), !, parse_(Ls).
parse_([]) --> [].
% }}}

get(K, Assoc0, Assoc, V) :-
  (   get_assoc(K, Assoc0, V)
  ->  Assoc = Assoc0
  ;   V in inf..sup,
      put_assoc(K, Assoc0, V, Assoc)
  ).

make_scope([], Res, Res) :- !.
make_scope([K-V|Rest], Acc0, Res) :-
  integer(V), !,
  get(K, Acc0, Acc, Value),
  Value #= V,
  make_scope(Rest, Acc, Res).
make_scope([K-V0|Rest], Acc0, Res) :-
  V0 =.. [O, Op1, Op2],
  get(Op1, Acc0, Acc1, V1),
  get(Op2, Acc1, Acc2, V2),
  Operation =.. [O, V1, V2],
  get(K, Acc2, Acc, Value),
  Value #= Operation,
  make_scope(Rest, Acc, Res).

make_scope(KVs, Res) :-
  empty_assoc(Acc),
  make_scope(KVs, Acc, Res).

solve1(Data, Res) :-
  make_scope(Data, Scope),
  get(root, Scope, _, Res).

remove_root_humn(root-_).
remove_root_humn(humn-_).

solve2(Data, Res) :-
  once(member(root-RootExpr, Data)),
  exclude(remove_root_humn, Data, D1),
  RootExpr =.. [_, Left, Right],
  make_scope(D1, Scope0),
  get(Left, Scope0, Scope1, LVal),
  get(Right, Scope1, Scope2, RVal),
  LVal #= RVal,
  get(humn, Scope2, _, Res),
  once(labeling([min(Res)], [Res])).

input(Data) :-
  phrase_from_file(parse(Data), "inputs/21.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~d~n", [One]),
  solve2(Data, Two),
  format("Solution 2: ~d~n", [Two]).

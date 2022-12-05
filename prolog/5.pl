:- use_module(library(pio)).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(assoc)).
:- use_module(library(yall)).

% test {{{
test_data("    [D]   \x20
[N] [C]   \x20
[Z] [M] [P]
 1   2   3\x20

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2").

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
white --> [W], { nonvar(W), code_type(W, space), \+ code_type(W, newline) }.
whites --> white, !, whites.
whites --> [].

blank --> [C], { nonvar(C), code_type(C, space) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> [C], { code_type(C, newline) }.

eol, [0'\n] --> "\n".
eos([], []).

integer(I) -->
  digit(C), integer0(Cs),
  { number_codes(I, [C|Cs]) }.
integer0([C|Cs]) --> digit(C), !, integer0(Cs).
integer0([]) --> [].

digit(C) --> [C], { nonvar(C), code_type(C, digit) }.

crate(nil) --> "   ", !.
crate(C) -->
  "[", [C0], "]",
  { between(0'A, 0'Z, C0),
    char_code(C, C0) }.

crates_line([L|Ls]) --> crate(L), crates_line_(Ls).
crates_line_([L|Ls]) --> " ", crate(L), !, crates_line_(Ls).
crates_line_([]) --> whites, (eol | eos).

cargo(Ls) -->
  crates_line(L), cargo_(Ls0),
  { transpose([L|Ls0], Ls2),
    maplist(exclude(=(nil)), Ls2, Ls) }.
cargo_([L|Ls]) --> nl, crates_line(L), !, cargo_(Ls).
cargo_([]) --> [].

name(I) --> " ", integer(I), " ".

names([I|Is]) --> name(I), names_(Is), whites, (eol | eos).
names_([I|Is]) --> " ", name(I), !, names_(Is).
names_([]) --> [].

rule(A * C1 -> C2) -->
  "move ", integer(A),
  " from ", integer(C1),
  " to ", integer(C2),
  whites, (eos | eol).

rules([R|Rs]) --> rule(R), rules_(Rs).
rules_([R|Rs]) --> nl, rule(R), !, rules_(Rs).
rules_([]) --> [].

parse(Cargo -> Rules) -->
  cargo(Cargo0), nl, names(Names), !,
  { pairs_keys_values(Cargo1, Names, Cargo0),
    list_to_assoc(Cargo1, Cargo) },
  nl, nl, rules(Rules), blanks.
% }}}

first_from_all(Cargo, Res) :-
  assoc_to_values(Cargo, Vs),
  maplist(nth0(0), Vs, Res).

apply(Cargo, Rules, Move, Res) :-
  foldl(
    {Move}/[Times * L1 -> L2, Cargo0, O]>>call(Move, Cargo0, Times, L1, L2, O),
    Rules, Cargo, Cargo1),
  first_from_all(Cargo1, Res0),
  atom_chars(Res, Res0).

move1(Res, 0, _, _, Res) :- !.
move1(Cargo0, N0, L1, L2, Res) :-
  get_assoc(L1, Cargo0, [Crate|NewL1]),
  get_assoc(L2, Cargo0, NewL2_),
  NewL2 = [Crate|NewL2_],

  put_assoc(L1, Cargo0, NewL1, Cargo1),
  put_assoc(L2, Cargo1, NewL2, Cargo),

  succ(N, N0),

  move1(Cargo, N, L1, L2, Res).

move2(Cargo0, N, L1, L2, Res) :-
  get_assoc(L1, Cargo0, OldL1),
  get_assoc(L2, Cargo0, OldL2),

  length(Buf, N),
  once(append(Buf, NewL1, OldL1)),
  append(Buf, OldL2, NewL2),

  put_assoc(L1, Cargo0, NewL1, Cargo1),
  put_assoc(L2, Cargo1, NewL2, Res).

solve1(Cargo -> Rules, Res) :- apply(Cargo, Rules, move1, Res).
solve2(Cargo -> Rules, Res) :- apply(Cargo, Rules, move2, Res).

input(Data) :-
  phrase_from_file(parse(Data), "inputs/5.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 1: ~w~n", [Two]).

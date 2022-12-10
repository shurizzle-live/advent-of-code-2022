:- use_module(library(pio)).
:- use_module(library(ordsets)).
:- use_module(util).

test_input_("R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2").
test_input2_("R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20").

test_input(I) :-
  test_input_(I0),
  string_codes(I0, I).
test_input2(I) :-
  test_input2_(I0),
  string_codes(I0, I).

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

direction(up) --> "U".
direction(down) --> "D".
direction(left) --> "L".
direction(right) --> "R".

instruction((D * T)) --> direction(D), " ", integer(T).

parse([I|Is]) --> blanks, instruction(I), parse_(Is), blanks.
parse_([I|Is]) --> nl, instruction(I), !, parse_(Is).
parse_([]) --> [].
% }}}

unroll_instructions([(D * T)|_], Dir) :- unroll_instructions_(D, T, Dir).
unroll_instructions([_|Rest], Dir) :- unroll_instructions(Rest, Dir).
unroll_instructions_(_, 0, _) :- !, fail.
unroll_instructions_(D, _, D).
unroll_instructions_(D, T0, Dir) :-
  T is T0-1,
  unroll_instructions_(D, T, Dir).

move((X0, Y), up,    (X, Y)) :- !, X is X0+1.
move((X0, Y), down,  (X, Y)) :- !, X is X0-1.
move((X, Y0), left,  (X, Y)) :- !, Y is Y0-1.
move((X, Y0), right, (X, Y)) :- !, Y is Y0+1.

distance((X1, Y1), (X2, Y2), Res) :-
  Res is floor(sqrt((X1-X2)^2 + (Y1-Y2)^2)).

clamp(Min, Max, V0, V) :- V is min(Max, max(Min, V0)).

follow((X1, Y1), (X2, Y2), (X, Y)) :-
      distance((X1,Y1), (X2,Y2), D),
      D > 1
  ->  clamp(-1, 1, X2-X1, X3),
      clamp(-1, 1, Y2-Y1, Y3),
      X is X1 + X3,
      Y is Y1 + Y3
  ;   X is X1,
      Y is Y1.

evolve(Dir, (Pos20, Pos100, [H0|Points0]), (Pos2, Pos10, Points)) :-
  move(H0, Dir, H),
  scanl(follow, Points0, H, Points),
  nth1(2, Points, P2),
  nth1(10, Points, P10),
  ord_add_element(Pos20, P2, Pos2),
  ord_add_element(Pos100, P10, Pos10).

make_state(P, Knots, State) :-
  length(Ns, Knots),
  maplist(=(P), Ns),
  State = ([P], [P], Ns).

initial_state(State) :- make_state((0, 0), 10, State).

solve1(Data, Res) :-
  initial_state(S0),
  foldall(evolve, Dir, unroll_instructions(Data, Dir), S0, (Pos, _, _)),
  length(Pos, Res).

solve2(Data, Res) :-
  initial_state(S0),
  foldall(evolve, Dir, unroll_instructions(Data, Dir), S0, (_, Pos, _)),
  length(Pos, Res).

input(Data) :-
  phrase_from_file(parse(Data), "inputs/9.txt").

solution :-
  input(Data),

  initial_state(S0),
  foldall(evolve, Dir, unroll_instructions(Data, Dir), S0, (Pos2, Pos10, _)),
  length(Pos2, One),
  length(Pos10, Two),

  format("Solution 1: ~w~n", [One]),
  format("Solution 1: ~w~n", [Two]).

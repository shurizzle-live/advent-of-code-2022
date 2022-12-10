:- use_module(library(pio)).
:- use_module(util).

test_data(Data) :- phrase_from_file(parse(Data), "inputs/10-test.txt").

test_input_("noop
addx 3
addx -5").

test_input(I) :-
  test_input_(I0),
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

sign_integer(I) --> "-", !, integer(I0), { I is -I0 }.
sign_integer(I) --> "+", !, integer(I).
sign_integer(I) --> integer(I).

op_(noop) --> "noop", !.
op_(addx(I)) --> "addx ", sign_integer(I).

parse([Op|Ops]) --> blanks, op_(Op), parse_(Ops), blanks.
parse_([Op|Ops]) --> nl, op_(Op), !, parse_(Ops).
parse_([]) --> [].
% }}}

noop(_, I, X, I, X).
noop(Ops, I0, X0, I, X) :-
  I1 is I0+1,
  run(Ops, I1, X0, I, X).

addx(_, _, I, X, I, X).
addx(_, _, I0, X, I, X) :- I is I0+1.
addx(A, Ops, I0, X0, I, X) :-
  I1 is I0+2,
  X1 is X0+A,
  run(Ops, I1, X1, I, X).

run([], I, X, I, X).
run([Op|Ops], I0, X0, I, X) :-
  call(Op, Ops, I0, X0, I, X).

run(Ops, I, X) :- run(Ops, 0, 1, I, X).

indexes(20).
indexes(60).
indexes(100).
indexes(140).
indexes(180).
indexes(220).

indexes((I0, X), Acc0, Acc) :-
  I1 is I0+1,
  (   indexes(I1)
  ->  Acc is Acc0+I1*X
  ;   Acc is Acc0
  ).

solve1(Data, Res) :-
  foldall(indexes, (I,X), run(Data, I, X), 0, Res).

write_canvas((I0, X), Canvas0, Canvas) :-
  Pos is I0 mod 40,
  (   Start is max(0, X-1),
      Stop is min(X+1, 39),
      between(Start, Stop, Pos)
  ->  replace0(I0, Canvas0, 0'#, Canvas)
  ;   Canvas = Canvas0
  ).

canvas_line_size(L) :- length(L, 40).
codes_string(Codes, String) :- string_codes(String, Codes).

nl_joiner(S1, S2, S) :-
  string_concat(S2, "\n", S3),
  string_concat(S3, S1, S).

canvas_string(Canvas, String) :-
  length(Lines0, 6),
  maplist(canvas_line_size, Lines0),
  append(Lines0, Canvas),
  maplist(codes_string, Lines0, Lines),
  foldl1(nl_joiner, Lines, String).

solve2(Data, Res) :-
  L is 40*6,
  length(Canvas0, L),
  maplist(=(0' ), Canvas0),
  foldall(write_canvas, (I,X), run(Data, I, X), Canvas0, Canvas),
  canvas_string(Canvas, Res).

input(Data) :-
  phrase_from_file(parse(Data), "inputs/10.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 2:~n~s~n", [Two]).

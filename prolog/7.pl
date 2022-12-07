:- use_module(library(pio)).
:- use_module(library(assoc)).

test_input_("$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k").

test_input(I) :-
  test_input_(I0),
  string_codes(I0, I).

add_to_fs(State0, [], What, State) :- !,
  add_to_fs_(State0, What, State).
add_to_fs(State0, [Dir|T], What, State) :-
  (   get_assoc(Dir, State0, dir(State1))
  ;   empty_assoc(State1)
  ),
  add_to_fs(State1, T, What, State2),
  put_assoc(Dir, State0, dir(State2), State).

add_to_fs_(State, [], State) :- !.
add_to_fs_(State0, [Name|T], State) :-
  atom(Name), !,
  \+ get_assoc(Name, State0, _),
  empty_assoc(Children),
  put_assoc(Name, State0, dir(Children), State1),
  add_to_fs_(State1, T, State).
add_to_fs_(State0, [(Name, Size)|T], State) :-
  \+ get_assoc(Name, State0, _),
  put_assoc(Name, State0, file(Size), State1),
  add_to_fs_(State1, T, State).

% Definite clause grammar {{{
blank --> [C], { nonvar(C), code_type(C, space) }.
blanks --> blank, !, blanks.
blanks --> [].

white --> [C], { nonvar(C), code_type(C, space), \+ code_type(C, newline) }.
whites --> white, !, whites.
whites --> [].

nl --> "\n".

nonblank(C) --> [C], { nonvar(C), \+ code_type(C, space) }.
nonblanks([C|Cs]) --> nonblank(C), !, nonblanks(Cs).
nonblanks([]) --> [].

eol, [0'\n] --> "\n".
eos([], []).

eol_or_eos --> eol, !.
eol_or_eos --> eos.

integer(I) -->
  digit(C), integer0(Cs),
  { number_codes(I, [C|Cs]) }.
integer0([C|Cs]) --> digit(C), !, integer0(Cs).
integer0([]) --> [].

digit(C) --> [C], { nonvar(C), code_type(C, digit) }.

cd((_, Fs), ([], Fs)) -->
  "cd /", !,
  whites, eol_or_eos.
cd((Cwd0, Fs), (Cwd, Fs)) -->
  "cd ..", !,
  whites, eol_or_eos,
  { length(Cwd0, L0),
    succ(L, L0),
    length(Cwd, L),
    once(append(Cwd, _, Cwd0)) }.
cd((Cwd0, Fs), (Cwd, Fs)) -->
  "cd ", nonblanks(Dir0),
  whites, eol_or_eos,
  { atom_codes(Dir, Dir0),
    append(Cwd0, [Dir], Cwd) }.

ls_output_line(Name) -->
  "dir ", nonblanks(Name0), !,
  { atom_codes(Name, Name0) }.
ls_output_line((Name, Size)) -->
  integer(Size), " ", nonblanks(Name0), !,
  { atom_codes(Name, Name0) }.

ls_output([N|Ns]) -->
  nl,
  ls_output_line(N), !,
  ls_output(Ns).
ls_output([]) --> [].

ls((Cwd, Fs0), (Cwd, Fs)) -->
  "ls", !,
  whites, eol_or_eos,
  ls_output(Nodes),
  { add_to_fs(Fs0, Cwd, Nodes, Fs) }.

cmd(State0, State) --> "$ ", cd(State0, State), !.
cmd(State0, State) --> "$ ", ls(State0, State).

parse(Fs) -->
  { empty_assoc(Fs0) },
  cmd(([], Fs0), State0),
  parse_(State0, (_, Fs)),
  blanks.
parse_(State0, State) -->
  nl, cmd(State0, State1), !,
  parse_(State1, State).
parse_(State, State) --> [].
% }}}

size(file(Size), Size).
size(dir(Children0), Size) :-
  assoc_to_values(Children0, Children),
  maplist(size, Children, Sizes),
  sum_list(Sizes, Size).

dirs(Children0, Size) :-
  assoc_to_values(Children0, Children),
  dirs_(Children, Size).

dirs_([H|_], Size) :-
  dir(_) = H,
  size(H, Size).
dirs_([dir(Children)|_], Size) :-
  dirs(Children, Size).
dirs_([_|T], Size) :-
  dirs_(T, Size).

solve1(Fs, Res) :-
  aggregate_all(
    sum(Size),
    ( dirs(Fs, Size),
      Size =< 100000 ),
    Res
  ).
solve2(Fs, Res) :-
  size(dir(Fs), FsSize),
  RemainingSpace is 70000000 - FsSize,
  RequiredSpace is 30000000 - RemainingSpace,
  aggregate_all(
    min(Size),
    ( dirs(Fs, Size),
      Size >= RequiredSpace ),
    Res
  ).

input(Data) :-
  phrase_from_file(parse(Data), "inputs/7.txt").

solution1(Res) :- input(Data), solve1(Data, Res).

solution2(Res) :- input(Data), solve2(Data, Res).

solution :-
  input(Data),
  solve1(Data, One),
  format("Solution 1: ~w~n", [One]),
  solve2(Data, Two),
  format("Solution 1: ~w~n", [Two]).

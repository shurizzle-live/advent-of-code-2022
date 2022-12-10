:- module(util, [foldall/5]).

:- meta_predicate foldall(3, ?, 0, ?, ?).

foldall(Reducer, Template, Goal, V0, V) :-
  !,
  State = state(+V0),
  (  call(Goal),
         arg(1, State, +S0),
         (  call(Reducer, Template, S0, S)
         -> nb_setarg(1, State, +S)
         ;  nb_setarg(1, State, fail)
         ),
         fail
  ;  arg(1, State, +V)
  ).

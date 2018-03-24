:- module(church_encoding, [numeral_to_atom/2]).

:- use_module(interpret, [interpret/3]).
:- use_module(reduction, [b_reducetr/2]).
% Utilities for displaying Church encoded data


numeral_to_atom(N, A) :-
  b_reducetr(app(app(N, succ-1), zero-0), T),
  list_to_assoc([succ-[X, SX]>>(SX is X + 1), zero-0], Ms),
  once(interpret(T, Ms, A)).

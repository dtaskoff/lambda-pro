:- module(church_encoding, [numeral_to_atom/2]).

:- use_module(interpret, [interpret/3]).
:- use_module(reduction, [b_reducetr/2]).
% Utilities for displaying Church encoded data


numeral_to_atom(abs(F, abs(X, app(F-1, I, X-0))), I) :- !.
% ^ optimised for numerals using the itr functor
numeral_to_atom(N, A) :-
  b_reducetr(app(app(N, 1, succ-1), 1, zero-0), T),
  list_to_assoc([succ-[X, SX]>>(SX is X + 1), zero-0], Ms),
  once(interpret(T, Ms, A)).

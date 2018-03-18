:- module(helpers,
  [ atom_de_bruijn/2, atom_de_bruijn/3
  , atom_de_bruijn_atom/2
  ]).

:- use_module(terms).
:- use_module(indices).

atom_de_bruijn(A, N) :- atom_de_bruijn(A, _, N).

atom_de_bruijn(A, T, N) :- var(N), atom_term(A, T), term_de_bruijn(T, N), !.
atom_de_bruijn(A, T, N) :- term_de_bruijn(T, N), atom_term(A, T).

atom_de_bruijn_atom(A, NA) :- var(NA), atom_de_bruijn(A, N),
  atom_term(NA, N), !.
atom_de_bruijn_atom(A, NA) :- atom_term(NA, N),
  atom_de_bruijn(A, N).

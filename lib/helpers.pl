:- module(helpers,
  [ atom_de_bruijn/4, atom_de_bruijn/5
  , atom_de_bruijn_atom/4
  ]).

:- use_module(terms).
:- use_module(indices).

atom_de_bruijn(A, N, I, Ii) :- atom_de_bruijn(A, _, N, I, Ii).

atom_de_bruijn(A, T, N, I, Ii) :- nonvar(A), atom_term(A, T),
  term_de_bruijn(T, N, I, Ii), !.
atom_de_bruijn(A, T, N, I, I) :- term_de_bruijn(T, N, I, I),
  atom_term(A, T).

atom_de_bruijn_atom(A, NA, I, Ii) :- var(NA), atom_de_bruijn(A, N, I, Ii),
  atom_term(NA, N), !.
atom_de_bruijn_atom(A, NA, I, I) :- atom_term(NA, N),
  atom_de_bruijn(A, N, I, I).

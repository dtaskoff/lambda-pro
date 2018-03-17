:- module(main, [
  term/1, atom_term/2, term_de_bruijn/2,
  e_reduce/2, free_in/2
  ]).

:- use_module(lib/terms).
:- use_module(lib/indices).
:- use_module(lib/reduction).

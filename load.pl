:- module(main,
  [ term/1, atom_term/2, eq/2, free_variables/2
  , term_de_bruijn/4, index_of/4
  , b_reduce/2, substitute/4, up/3
  , e_reduce/2, free_in/2
  , atom_de_bruijn/4, atom_de_bruijn/5, atom_de_bruijn_atom/4
  ]).

:- use_module(lib/terms).
:- use_module(lib/indices).
:- use_module(lib/reduction).
:- use_module(lib/helpers).

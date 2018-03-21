:- module(main,
  [ term/1, term_to_atom/3, atom_to_term/5
  , index_of/3, eq/2, free_variables/2
  , b_reduce/2, substitute/4, up/3
  , e_reduce/2, free_in/2
  ]).

:- use_module(lib/terms).
:- use_module(lib/reduction).

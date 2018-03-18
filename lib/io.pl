:- module(io,
  [ read_lambda/2, write_binding/2
  ]).

:- use_module(terms, [atom_term/2]).
:- use_module(indices, [term_de_bruijn/2]).
:- use_module(utils, [atom_list_concat/2]).

% reads a user-friendly λ-term and stores it in both
% internal representations (with and without de Bruijn indices)
read_lambda(T, N) :- read_string(user_input, "\n", "\t", _, S),
  atom_term(S, T), term_de_bruijn(T, N).

% write_binding(Term, Name).
write_binding(T, N) :- atom_term(A, T),
  atom_list_concat([N, ' = ', A], O), writeln(O).

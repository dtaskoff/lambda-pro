:- module(evaluate,
  [ evaluate_input/6
  ]).

:- use_module(terms, [atom_term/2]).
:- use_module(indices, [term_de_bruijn/2]).
:- use_module(utils, [atom_list_concat/2]).

evaluate_input(In, Out, Bs, Ns, Bsi, Nsi) :-
  process_quit(In, Out);
  process_lambda(In, Out, Bs, Ns, Bsi, Nsi).

% Exit if the user has entered 'quit'
process_quit("quit", _) :- halt.

% Show a λ-term with its corresponding version
% with de Bruijn indices
process_lambda(A, Out, Bs, [N|Ns], Bsi, Ns) :-
  atom_term(A, T), term_de_bruijn(T, Ti),
  put_assoc(N, Bs, (T, Ti), Bsi),
  atom_term(Ai, Ti),
  atom_list_concat([N, ' = ', A, '\n', '(de Bruijn) ', Ai], Out).

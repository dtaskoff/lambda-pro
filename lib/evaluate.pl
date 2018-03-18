:- module(evaluate,
  [ evaluate_input/6
  ]).

:- use_module(terms, [atom_term/2]).
:- use_module(indices, [term_de_bruijn/2]).
:- use_module(reduction, [b_reduce/2, e_reduce/2]).
:- use_module(helpers, [atom_de_bruijn/2]).
:- use_module(utils, [atom_list_concat/2]).

evaluate_input(In, Out, Bs, Ns, Bsi, Nsi) :-
  process_quit(In, Out);
  process_reduction(In, Out, Bs, Ns, Bsi, Nsi, b_reduce);
  process_reduction(In, Out, Bs, Ns, Bsi, Nsi, e_reduce);
  process_lambda(In, Out, Bs, Ns, Bsi, Nsi);
  process_bad_input(In, Out).

% Exit if the user has entered 'quit'
process_quit("quit", _) :- halt.

% Show a λ-term with its corresponding version
% with de Bruijn indices
process_lambda(A, Out, Bs, [N|Ns], Bsi, Ns) :-
  atom_term(A, T), term_de_bruijn(T, Ti),
  put_assoc(N, Bs, (T, Ti), Bsi),
  atom_term(Ai, Ti),
  atom_list_concat([N, ' = ', A, '\n', '(de Bruijn) ', Ai], Out).

process_reduction(In, Outi, Bs, Ns, Bsi, Nsi, Reduce) :-
  x_reduction(Reduce, In, A), atom_de_bruijn(A, N),
  call(Reduce, N, Ni), atom_de_bruijn(Ai, Ni),
  process_lambda(Ai, Out, Bs, Ns, Bsi, Nsi),
  atom_reduce(R, Reduce),
  atom_list_concat([A, R, Ai, '\n', Out], Outi).

atom_reduce(' -β> ', b_reduce).
atom_reduce(' -η> ', e_reduce).

x_reduction(b_reduce, A, Ai) :- x_reduction(beta, A, Ai), !.
x_reduction(e_reduce, A, Ai) :- x_reduction(eta, A, Ai), !.
x_reduction(X, A, Ai) :-
  atom_chars(A, CS), once(phrase(x_reduction(X, As), CS)),
  atom_list_concat(As, Ai).

x_reduction(beta, A) --> [b, e, t, a, ' '|A].
x_reduction(eta, A) --> [e, t, a, ' '|A].

% Dislay a message if the input isn't valid
process_bad_input(In, Out) :-
  atom_list_concat(['Bad input: ', In], Out).

:- module(evaluate,
  [ evaluate_input/8
  ]).

:- use_module(terms,
  [ term_to_atom/3, atom_to_term/5
  , index_of/3, eq/2, free_variables/2]).
:- use_module(reduction, [b_reduce/2, e_reduce/2, substitute/4]).
:- use_module(utils, [atom_list_concat/2]).

evaluate_input(In, Out, Bs, Bsi, Ns, Nsi, I, Ii) :-
  evaluate_quit(In, Out);
  evaluate_reduction(In, Out, Bs, Bsi, Ns, Nsi, I, Ii, _);
  evaluate_equivalence(In, Out, Bs, Bsi, Ns, Nsi, I, Ii);
  evaluate_lambda(In, Out, Bs, Bsi, Ns, Nsi, I, Ii);
  evaluate_bad_input(In, Out), Nsi = Ns, Bsi = Bs, Ii = I.

% Exit if the user has entered 'quit'
evaluate_quit(quit, _) :- halt.

% Store and show a λ-term with its corresponding name and
% version with de Bruijn indices
%
% Look if A is a name bound in the environment
evaluate_lambda(A, Out, Bs, Bs, Ns, Ns, I, I) :-
  get_assoc(A, Bs, T),
  term_to_atom(T, Ai, normal), term_to_atom(T, Aii, de_bruijn),
  show_terms(A, Ai, Aii, Out), !.
% Add a new λ-term to the environment
evaluate_lambda(A, Out, Bs, Bsi, Ns, Nsi, I, Ii) :-
  evaluate_(A, Out, Bs, Bsi, Ns, Nsi, I, Ii).

% This is used to optimise the number of conversions
% between the formats
evaluate_(A, Out, Bs, Bsi, [N|Ns], Ns, I, Ii) :-
  atom_to_term(A, T, normal, I, Ii),
  put_assoc(N, Bs, T, Bsi),
  term_to_atom(T, Ai, de_bruijn),
  show_terms(N, A, Ai, Out).

% Show a λ-term with its corresponding name and
% version with de Bruijn indices
show_terms(N, A, Ai, S) :-
  atom_list_concat([N, ' = ', A, '\n(de Bruijn) ', Ai], S).

evaluate_reduction(In, Outi, Bs, Bsi, Ns, Nsi, I, Iii, Reduce) :-
  x_reduction(Reduce, In, A),
  (evaluate_substitutions(A, T, S, Bs, I, _) -> true;
    atom_to_term(A, T, normal, I, _), S = ''),
  call(Reduce, T, Ti),
  term_to_atom(Ti, Ai, normal),
  evaluate_(Ai, Out, Bs, Bsi, Ns, Nsi, I, Iii),
  atom_reduce(R, Reduce),
  atom_list_concat([S, '\n', R, Ai, '\n', Out], Outi).

% Substitute all free variables in an atom A
% that are bound in the environment
evaluate_substitutions(A, Mi, Out, Bs, I, Iii) :-
  evaluate_substitution(A, M, Bs, I, Ii),
  term_to_atom(M, Ai, normal),
  (evaluate_substitutions(Ai, Mi, S, Bs, Ii, Iii) ->
    atom_list_concat([A, ' =α= ', S], Out);
    Mi = M, atom_list_concat([A, ' =α= ', Ai], Out)).

% Substitute the first free variable in an atom A
% that is bound in the environment
evaluate_substitution(A, M, Bs, I, Ii) :-
  atom_to_term(A, T, normal, I, Ii),
  free_variables(T, V), memberchk(X-_, V),
  get_assoc(X, Bs, N), substitute(T, X, N, M).

atom_reduce(' -β> ', b_reduce).
atom_reduce(' -η> ', e_reduce).

x_reduction(X, A, Ai) :-
  atom_chars(A, CS), once(phrase(x_reduction(X, As), CS)),
  atom_list_concat(As, Ai).

x_reduction(b_reduce, A) --> [b, e, t, a, ' '|A].
x_reduction(e_reduce, A) --> [e, t, a, ' '|A].

evaluate_equivalence(In, Out, Bs, Ns, Ns, Bs, I, I) :-
  equivalence(In, Mf, Nf),
  get_assoc(Mf, Bs, M), get_assoc(Nf, Bs, N),
  term_to_atom(M, MA, normal), term_to_atom(N, NA, normal),
  (eq(M, N) -> Eq = true; Eq = false),
  atom_list_concat([MA, ' =α= ', NA, ' ?\n', Eq], Out).

equivalence(A, M, N) :-
  atom_chars(A, CS), once(phrase(equivalence(Ms, Ns), CS)),
  atom_list_concat(Ms, M), atom_list_concat(Ns, N).

equivalence([C|Cs], N) --> [C], equivalence(Cs, N), { C \= ' ' }.
equivalence([], N) --> [' ', =, =, ' '|N].

% Dislay a message if the input isn't valid
evaluate_bad_input(In, Out) :- atom_concat('Bad input: ', In, Out).

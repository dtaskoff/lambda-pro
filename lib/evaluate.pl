:- module(evaluate, [evaluate_input/4]).


:- use_module(terms,
  [ term_to_atom/3, atom_to_term/5
  , index_of/3, eq/2, free_variables/2]).
:- use_module(reduction,
    [ b_reduce/2, b_reducetr/2
    , e_reduce/2, e_reducetr/2, substitute/4]).
:- use_module(church_encoding, [numeral_to_atom/2]).
:- use_module(utils, [atom_list_concat/2]).
:- use_module(io, [read_file/2]).


% evaluate_input(Input, Output, StateIn, StateOut), where
% State = (Bindings, Names, NextIndex)
evaluate_input(In, Out, S, Si) :-
  evaluate_quit(In, Out), Si = S;
  skip_comment(In, Out), Si = S;
  evaluate_numeral(In, Out, S, Si);
  evaluate_load(In, Out, S, Si);
  evaluate_name_binding(In, Out, S, Si);
  evaluate_reduction(In, Out, S, Si);
  evaluate_equivalence(In, Out, S, Si);
  evaluate_lambda(In, Out, S, Si);
  evaluate_bad_input(In, Out), Si = S.

% Exit if the user has entered 'quit'
evaluate_quit(quit, _) :- halt.

% Skip a line starting with '%'
skip_comment(In, '') :- sub_atom(In, 0, 1, _, '%').

% Evaluate a Church numeral
evaluate_numeral(In, Out, S, S) :-
  sub_atom(In, 0, 1, _, '#'), sub_atom(In, 1, _, 0, N),
  S = (Bs, _, _), get_assoc(N, Bs, T), numeral_to_atom(T, A),
  term_to_atom(T, Ai, de_bruijn), show_terms(N, A, Ai, Out).

% Load a file into the repl
evaluate_load(In, Out, S, Si) :- file_to_load(In, F), load_file(F, Out, S, Si).

load_file(F, Out, S, Si) :- read_file(F, Lines),
  foldl([Line, (Outii, Sii), (Outiv, Siii)]>>
    (evaluate_input(Line, Outiii, Sii, Siii),
      atom_list_concat([Outii, '\n', Outiii], Outiv)),
    Lines, ('', S), (Out, Si)).

file_to_load(A, F) :-
  atom_chars(A, Cs), once(phrase(file_to_load(Fs), Cs)),
  atom_list_concat(Fs, F).

file_to_load(F) --> [l, o, a, d, ' '|F].

% Bind a term to a name. If the name is already used,
% display an 'error' message
evaluate_name_binding(In, Out, (Bs, Ns, I), Si) :-
  name_binding(In, N, B),
  (get_assoc(N, Bs, _) ->
    atom_list_concat(['`', N, '` is already bound'], Msg),
    Si = (Bs, Ns, I),
    evaluate_bad_input(Msg, Out);
    evaluate_input(B, Out, (Bs, [N|Ns], I), Si)).

name_binding(A, N, B) :-
  atom_chars(A, Cs), once(phrase(name_binding(Ns, Bs), Cs)),
  atom_list_concat(Ns, N), atom_list_concat(Bs, B).

name_binding([C|Cs], B) --> [C], name_binding(Cs, B), { C \= ' ' }.
name_binding([], B) --> [' ', =, ' '|B].

% Store and show a λ-term with its corresponding name and
% version with de Bruijn indices
%
% If A is x?, look if x is a name bound in the environment,
% else add a new λ-term to the environment
evaluate_lambda(In, Out, S, Si) :-
  sub_atom(In, _, 1, 0, End),
  End == ? -> Si = S,
    sub_atom(In, 0, _, 1, N),
    S = (Bs, _, _),
    (get_assoc(N, Bs, T) ->
      term_to_atom(T, A, normal),
      term_to_atom(T, Ai, de_bruijn),
      show_terms(N, A, Ai, Out);
      atom_list_concat(['`', N, '` is not defined'], Msg),
      evaluate_bad_input(Msg, Out));
    evaluate_(In, Out, S, Si).

% This is used to optimise the number of conversions
% between the formats
evaluate_(A, Out, (Bs, [N|Ns], I), (Bsi, Ns, Ii)) :-
  atom_to_term(A, T, normal, I, Ii),
  put_assoc(N, Bs, T, Bsi),
  term_to_atom(T, Ai, de_bruijn),
  show_terms(N, A, Ai, Out).

% Show a λ-term with its corresponding name and
% version with de Bruijn indices
show_terms(N, A, Ai, S) :-
  atom_list_concat([N, ' = ', A, '\n(de Bruijn) ', Ai], S).

evaluate_reduction(In, Out, S, Si) :-
  x_reduction(Reduce, In, A),
  S = (Bs, _, I),
  (evaluate_substitutions(A, T, Subs, Bs, I, _) -> true;
    atom_to_term(A, T, normal, I, _), Subs = ''),
  call(Reduce, T, Ti),
  term_to_atom(Ti, Ai, normal),
  evaluate_(Ai, Outi, S, Si),
  atom_reduce(R, Reduce),
  atom_list_concat([Subs, '\n', R, Ai, '\n', Outi], Out).

% Substitute all free variables in an atom A
% that are bound in the environment
evaluate_substitutions(A, Mi, Out, Bs, I, Iii) :-
  evaluate_substitution(A, M, Bs, I, Ii),
  term_to_atom(M, Ai, normal),
  (evaluate_substitutions(Ai, Mi, S, Bs, Ii, Iii) ->
    atom_list_concat([A, '\n =ρ= ', S], Out);
    Mi = M, atom_list_concat([A, '\n =ρ= ', Ai], Out)).

% Substitute the first free variable in an atom A
% that is bound in the environment
evaluate_substitution(A, M, Bs, I, Ii) :-
  atom_to_term(A, T, normal, I, Ii),
  free_variables(T, V), memberchk(X-_, V),
  get_assoc(X, Bs, N), substitute(T, X, N, M).

atom_reduce(' -β> ', b_reduce).
atom_reduce(' -β>> ', b_reducetr).
atom_reduce(' -η> ', e_reduce).
atom_reduce(' -η>> ', e_reducetr).

x_reduction(X, A, Ai) :-
  atom_chars(A, CS), once(phrase(x_reduction(X, As), CS)),
  atom_list_concat(As, Ai).

x_reduction(b_reduce, A) --> [b, e, t, a, ' '|A].
x_reduction(b_reducetr, A) --> [b, e, t, a, *, ' '|A].
x_reduction(e_reduce, A) --> [e, t, a, ' '|A].
x_reduction(e_reducetr, A) --> [e, t, a, *, ' '|A].

evaluate_equivalence(In, Out, S, S) :-
  equivalence(In, Mf, Nf),
  S = (Bs, _, I),
  get_assoc(Mf, Bs, M), get_assoc(Nf, Bs, N),
  term_to_atom(M, MA, normal), term_to_atom(N, NA, normal),
  atom_to_term(MA, Mn, normal, I, _),
  atom_to_term(NA, Nn, normal, I, _),
  (eq(Mn, Nn) -> Eq = true; Eq = false),
  atom_list_concat([MA, ' =α= ', NA, ' ?\n', Eq], Out).

equivalence(A, M, N) :-
  atom_chars(A, CS), once(phrase(equivalence(Ms, Ns), CS)),
  atom_list_concat(Ms, M), atom_list_concat(Ns, N).

equivalence([C|Cs], N) --> [C], equivalence(Cs, N), { C \= ' ' }.
equivalence([], N) --> [' ', =, =, ' '|N].

% Dislay a message if the input isn't valid
evaluate_bad_input(In, Out) :- atom_concat('Bad input: ', In, Out).

:- module(evaluate, [evaluate_input/6]).


:- use_module(terms,
  [ term_to_atom/3, atom_to_term/5
  , eq/2, free_variables/2]).
:- use_module(reduction,
    [ b_reduce/2, b_reducetr/2
    , e_reduce/2, e_reducetr/2, substitute/4]).
:- use_module(church_encoding, [numeral_to_atom/2]).
:- use_module(utils, [atom_list_concat/2]).
:- use_module(io, [read_file/2]).


% evaluate_input(Input, Output, StateIn, StateOut, FlagsIn, FlagsOut), where
% State = (Bindings, Names, NextIndex)
evaluate_input(In, Out, Sin, Sout, Fin, Fout) :-
  evaluate_quit(In, Out, Sin, Sout, Fin, Fout);
  evaluate_env(In, Out, Sin, Sout, Fin, Fout);
  skip_comment(In, Out, Sin, Sout, Fin, Fout);
  evaluate_numeral(In, Out, Sin, Sout, Fin, Fout);
  evaluate_load(In, Out, Sin, Sout, Fin, Fout);
  evaluate_reload(In, Out, Sin, Sout, Fin, Fout);
  evaluate_name_binding(In, Out, Sin, Sout, Fin, Fout);
  evaluate_reduction(In, Out, Sin, Sout, Fin, Fout);
  evaluate_equivalence(In, Out, Sin, Sout, Fin, Fout);
  evaluate_term(In, Out, Sin, Sout, Fin, Fout);
  evaluate_bad_input(In, Out, Sin, Sout, Fin, Fout).

% Exit if the user has entered 'quit'
evaluate_quit(quit, quit, S, S, F, F) :- halt.

% Show the names in the current environment
evaluate_env(env, Out, S, S, F, F) :-
  S = (Bs, _, _), assoc_to_list(Bs, L),
  foldl([N-T, Out1, Out2]>>(
    term_to_atom(T, A, normal), show_term(N, A, Out),
    atom_list_concat([Out1, '\n', Out], Out2)
    ), L, 'Names: ', Out).

% Skip a line starting with '%'
skip_comment(In, '', S, S, F, F) :- sub_atom(In, 0, 1, _, '%').

% Evaluate a Church numeral
evaluate_numeral(In, Out, S, S, F, F) :-
  sub_atom(In, 0, 1, _, '#'), sub_atom(In, 1, _, 0, N),
  S = (Bs, _, _), get_assoc(N, Bs, T), numeral_to_atom(T, A),
  show_term(N, A, Out).

% Load a file into the repl
evaluate_load(In, Out, Sin, Sout, Fin, Fout) :-
  file_to_load(In, N), load_file(N, Out, Sin, Sout, Fin, Fout).

load_file(N, Out, Sin, Sout, Fin, Fout) :-
  catch((read_file(N, Lines), union([overwrite, file-N], Fin, F1),
    fold_with_flags(Lines, evaluate_input, _, Sin, Sout, F1, F2),
    subtract(F2, [overwrite], Fout), Out = ''), _,
    (atom_concat('Can\'t load file: ', N, Out), Sout = Sin, Fout = Fin)).

file_to_load(A, F) :-
  atom_chars(A, Cs), once(phrase(file_to_load(Fs), Cs)),
  atom_list_concat(Fs, F).

file_to_load(F) --> [l, o, a, d, ' '|F].

% Reload files that were loaded before
evaluate_reload(reload, Out, Sin, Sout, Fin, Fout) :-
  convlist([X, N]>>(X = file-N), Fin, Files),
  fold_with_flags(Files, load_file, Out, Sin, Sout, Fin, Fout).

% Abstract the folding over files and lines which collects flags
fold_with_flags(Xs, Func, Out, Sin, Sout, Fin, Fout) :-
  foldl([X, (Out1, S1, F1), (Out2, S2, F)]>>(
    call(Func, X, Result, S1, S2, Fin, F2),
    atom_list_concat([Out1, '\n', Result], Out2),
    union(F1, F2, F)), Xs, ('', Sin, Fin), (Out, Sout, Fout)).

% Bind a term to a name. If the name is already used,
% display an 'error' message
%
% Note: names can be overwritten by adding 'overwrite' to Flags
evaluate_name_binding(In, Out, (Bsin, Ns, I), Sout, F, F) :-
  name_binding(In, N, B, Ty),
  (memberchk(overwrite, F) ->
    (del_assoc(N, Bsin, _, Bsout) -> true; Bsout = Bsin); Bsout = Bsin),
  (get_assoc(N, Bsout, _) ->
    atom_list_concat(['`', N, '` is already bound'], Msg),
    Sout = (Bsin, Ns, I),
    evaluate_bad_input(Msg, Out, Sout, Sout, F, F);
    (Ty == eqv -> atom_concat('beta* ', B, BB); BB = B),
    evaluate_input(BB, Out, (Bsout, [N|Ns], I), Sout, F, F)).

name_binding(A, N, B, Ty) :-
  atom_chars(A, Cs), once(phrase(name_binding(Ns, Bs, Ty), Cs)),
  atom_list_concat(Ns, N), atom_list_concat(Bs, B).

name_binding([C|Cs], B, Ty) --> [C], { C \= ' '}, name_binding(Cs, B, Ty).
name_binding([], B, eqv) --> [' ', =, ' '|B].
name_binding([], B, def) --> [' ', :, =, ' '|B].

% Store and/or show a λ-term with its corresponding name and
% version with de Bruijn indices
%
% If A is x?, look if x is a name bound in the environment,
% else add a new λ-term to the environment
evaluate_term(In, Out, Sin, Sout, F, F) :-
  sub_atom(In, _, 1, 0, ?) -> Sout = Sin,
    sub_atom(In, 0, _, 1, N),
    Sin = (Bs, _, _),
    (get_assoc(N, Bs, T) ->
      term_to_atom(T, A, normal),
      term_to_atom(T, A1, de_bruijn),
      show_terms(N, A, A1, Out);
      atom_list_concat(['`', N, '` is not defined'], Msg),
      evaluate_bad_input(Msg, Out, Sin, Sout, F, F));
    evaluate_(In, _, Out, Sin, Sout).

% This is used to optimise the number of conversions
% between the formats
evaluate_(A, T, Out, (Bs, [N|Ns], I), (Bs1, Ns, I1)) :-
  (nonvar(A) -> atom_to_term(A, T, normal, I, I1);
    term_to_atom(T, A, normal), I1 = I),
  put_assoc(N, Bs, T, Bs1),
  show_term(N, A, Out).

% Show a λ-term with its corresponding name and
% version with de Bruijn indices
show_terms(N, A, A1, S) :-
  show_term(N, A, S1), atom_list_concat([S1, '\n', A1], S).

% Show a λ-term without its indices
show_term(N, A, S) :- atom_list_concat([N, ' = ', A], S).

evaluate_reduction(In, Out, Sin, Sout, F, F) :-
  x_reduction(Reduce, In, A),
  Sin = (Bs, _, I),
  atom_to_term(A, T, normal, I, _),
  (evaluate_substitutions(T, M, Bs) -> true; M = T),
  call(Reduce, M, T1),
  evaluate_(_, T1, Out1, Sin, Sout),
  atom_reduce(R, Reduce),
  atom_list_concat([R, '\n', Out1], Out).

% Substitute all free variables in an atom A
% that are bound in the environment
evaluate_substitutions(Tin, Tout, Bs) :-
  evaluate_substitution(Tin, M, Bs), !,
  (evaluate_substitutions(M, Tout, Bs) -> true; Tout = M).

% Substitute the first free variable in an atom A
% that is bound in the environment
evaluate_substitution(T, M, Bs) :-
  free_variables(T, V), member(X-_, V),
  get_assoc(X, Bs, N), substitute(T, X, N, M).

atom_reduce(' -β> ', b_reduce).
atom_reduce(' -β>> ', b_reducetr).
atom_reduce(' -η> ', e_reduce).
atom_reduce(' -η>> ', e_reducetr).

x_reduction(X, Ain, Aout) :-
  atom_chars(Ain, CS), once(phrase(x_reduction(X, As), CS)),
  atom_list_concat(As, Aout).

x_reduction(b_reduce, A) --> [b, e, t, a, ' '|A].
x_reduction(b_reducetr, A) --> [b, e, t, a, *, ' '|A].
x_reduction(e_reduce, A) --> [e, t, a, ' '|A].
x_reduction(e_reducetr, A) --> [e, t, a, *, ' '|A].

evaluate_equivalence(In, Out, S, S, F, F) :-
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

equivalence([C|Cs], N) --> [C], { C \= ' ' }, equivalence(Cs, N).
equivalence([], N) --> [' ', =, =, ' '|N].

% Dislay a message if the input isn't valid
evaluate_bad_input(In, Out, S, S, F, F) :- atom_concat('Bad input: ', In, Out).

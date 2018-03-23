:- module(repl, [repl/0]).

:- use_module(io, [set_prompt/0, read_input/1, print_output/1]).
:- use_module(evaluate, [evaluate_input/6]).


% The maximum number of names that could be defined in the repl
max_names(420).

% The maximum number of free variables in the repl's environment
max_index(4200).

% Generate a list of all names to be used in the repl
f_i(Fi, I) :- atom_concat(f, I, Fi).
fs(Fs) :- max_names(N), setof(X, between(0, N, X), Is),
  maplist(f_i, Fs, Is).

% repl(Bindings, Names, NextIndex).
% a simple read-evaluate-print loop
repl(S, F) :- read_input(In),
  evaluate_input(In, Out, S, Si, F, Fi),
  print_output(Out),
  repl(Si, Fi).

repl :- set_prompt, empty_assoc(Bs), fs(Fs), max_index(I), repl((Bs, Fs, I), []).

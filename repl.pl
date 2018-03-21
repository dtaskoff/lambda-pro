:- use_module(lib/io, [set_prompt/0, read_input/1, print_output/1]).
:- use_module(lib/evaluate, [evaluate_input/4]).


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
repl(S) :- read_input(In),
  evaluate_input(In, Out, S, Si),
  print_output(Out),
  repl(Si).

repl :- empty_assoc(Bs), fs(Fs), max_index(I), repl((Bs, Fs, I)).

:- set_prompt.
:- repl.

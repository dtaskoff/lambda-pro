:- use_module(lib/io, [set_prompt/0, read_input/1, print_output/1]).
:- use_module(lib/evaluate, [evaluate_input/6]).


% The maximum index of a name defined in the repl
max_index(420).

% Generate a list of all names to be used in the repl
f_i(Fi, I) :- atom_concat(f, I, Fi).
fs(Fs) :- max_index(I), setof(X, between(0, I, X), Is),
  maplist(f_i, Fs, Is).

% repl(Bindings, Names).
% a simple read-evaluate-print loop
repl(Bs, Ns) :- read_input(In),
  evaluate_input(In, Out, Bs, Ns, Bsi, Nsi),
  print_output(Out),
  repl(Bsi, Nsi).

repl :- empty_assoc(Bs), fs(Fs), repl(Bs, Fs).

:- set_prompt.
:- repl.

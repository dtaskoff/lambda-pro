:- use_module(lib/io,
  [ read_lambda/2, write_binding/2
  ]).

set_prompt :- prompt(_, 'λ-προ: ').

max_index(420).

f_i(Fi, I) :- atom_concat(f, I, Fi).
fs(Fs) :- max_index(I), setof(X, between(0, I, X), Is),
  maplist(f_i, Fs, Is).

% repl(Bindings, Names).
repl(Bs, [N|Ns]) :- read_lambda(T, Ti),
  put_assoc(N, Bs, (T, Ti), Bsi),
  write_binding(T, N),
  write_binding(Ti, '(de Bruijn)'),
  repl(Bsi, Ns).

repl :- empty_assoc(Bs), fs(Fs), repl(Bs, Fs), !.
repl :- halt.

:- set_prompt.
:- repl.

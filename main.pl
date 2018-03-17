% What is a lambda term?
lterm(X) :- v(X).                             % a variable
lterm(application(M, N)) :- term(M), term(N). % an application
lterm(lambda(X, M)) :- v(X), term(M).         % a lambda abstraction
lterm(parentheses(T)) :- term(T).

:- use_module(utils, [atom_list_concat/2]).

lterm_to_atom(X, X) :- v(X).
lterm_to_atom(application(M, N), S) :- lterm_to_atom(M, MS), lterm_to_atom(N, NS),
  atom_list_concat([MS, ' ', NS], S).
lterm_to_atom(lambda(X, M), S) :- lterm_to_atom(M, MS),
  atom_list_concat([X, '. ', MS], S).
lterm_to_atom(parentheses(T), S) :- lterm_to_atom(T, TS),
  atom_list_concat(['(', TS, ')'], S).

% The set of allowed variables
v(x). v(y). v(z). v(u). v(v). v(w).
variables(V) :- bagof(X, v(X), V).

% Convert a user-friendly lambda term into an internally represented lambda term
% term ::= abstraction | application | variable | (term)
% abstraction ::= variable. term
% application ::= term term
% variable ::= x | y | z | u | v | w
atom_to_lterm(A, T) :- atom_chars(A, CS), parse(CS, T).
string_to_lterm(S, T) :- string_chars(S, CS), parse(CS, T).

parse(CS, T) :- once(phrase(term(T), CS)).

term(T) --> lambda(T) | application(T) | variable(T) | parentheses(T).
lambda(lambda(X, M)) --> variable(X), ['.', ' '], term(M).
application(application(M, N)) --> lambda(M), [' '], term(N).
application(application(M, N)) --> variable(M), [' '], term(N).
application(application(M, N)) --> parentheses(M), [' '], term(N).
variable(X) --> [C], { atom_string(X, C), v(X) }.
parentheses(parentheses(T)) --> ['('], term(T), [')'].

% What is a 'de Bruijn' term?
dbterm(X) :- number(X).
dbterm(application(M, N)) :- dbterm(M), dbterm(N).
dbterm(lambda(M)) :- dbterm(M).
dbterm(parentheses(T)) :- dbterm(T).

dbterm_to_atom(X, X) :- number(X).
dbterm_to_atom(application(M, N), S) :-
  dbterm_to_atom(M, MS), dbterm_to_atom(N, NS), atom_list_concat([MS, ' ', NS], S).
dbterm_to_atom(lambda(M), S) :-
  dbterm_to_atom(M, MS), atom_list_concat(['λ ', MS], S).
dbterm_to_atom(parentheses(T), S) :-
  dbterm_to_atom(T, TS), atom_list_concat(['(', TS, ')'], S).

% Call the helper function with initialised accumulators
dbterm_to_lterm(N, X) :- variables(V), empty_assoc(G),
  dbterm_to_lterm(N, X, 0, V, _, G, _).
% Convert a de Bruijn term into a lambda term:
%
% convert a bound variable
dbterm_to_lterm(N, X, L, V, V, G, G) :-
  number(N), Ni is N - L, get_assoc(Ni, G, X), !.
% convert a free variable
dbterm_to_lterm(N, X, L, [X|V], V, G, Gi) :-
  number(N), Ni is N - L, put_assoc(Ni, G, X, Gi).

dbterm_to_lterm(application(MN, NN), application(M, N), L, V, Vii, G, Gii) :-
  dbterm_to_lterm(MN, M, L, V, Vi, G, Gi),
  dbterm_to_lterm(NN, N, L, Vi, Vii, Gi, Gii).

dbterm_to_lterm(lambda(MN), lambda(X, M), L, V, Vii, G, Giii) :-
  dbterm_to_lterm(-1, X, L, V, Vi, G, Gi),
  Ni is -L-1, put_assoc(Ni, Gi, X, Gii), Li is L + 1,
  dbterm_to_lterm(MN, M, Li, Vi, Vii, Gii, Giii).

dbterm_to_lterm(parentheses(MN), parentheses(M), L, V, Vi, G, Gi) :-
  dbterm_to_lterm(MN, M, L, V, Vi, G, Gi).

% Call the helper function with initialised accumulators
lterm_to_dbterm(X, N) :- empty_assoc(G),
  lterm_to_dbterm(X, N, 0, 42, _, G, _).
% Convert a lambda term into a de Bruijn term:
%
% convert a variable, which already has a de Bruijn index
lterm_to_dbterm(X, N, L, K, K, G, G) :-
  v(X), get_assoc(X, G, Ni), N is Ni + L, !.
% convert a variable, which doesn't have a de Bruijn index yet
lterm_to_dbterm(X, N, L, K, Ki, G, Gi) :-
  v(X), N is K + L, put_assoc(X, G, K, Gi), Ki is K - 1.

lterm_to_dbterm(application(M, N), application(MN, NN), L, K, Kii, G, Gii) :-
  lterm_to_dbterm(M, MN, L, K, Ki, G, Gi),
  lterm_to_dbterm(N, NN, L, Ki, Kii, Gi, Gii).

lterm_to_dbterm(lambda(X, M), lambda(MN), L, K, Ki, G, Giii) :-
  put_assoc(X, G, -1 - L, Gi),
  lterm_to_dbterm(M, MN, L + 1, K, Ki, Gi, Gii),
  del_assoc(X, Gii, -1 - L, Giii).
lterm_to_dbterm(parentheses(T), parentheses(TN), L, K, Ki, G, Gi) :-
  lterm_to_dbterm(T, TN, L, K, Ki, G, Gi).

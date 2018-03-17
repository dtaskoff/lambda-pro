% What is a λ-term?
% <λ-term> ::= <λ-abstraction> | <application> | <variable> | (<λ-term>)
% <λ-abstraction> ::= <variable>. <λ-term> | λ <λ-term>
% <application> ::= <λ-term> <λ-term>
% <variable> ::= s where s ∈ Σ⁺ | n where n ∈ N
term(X) :- v(X).
term(X) :- number(X).
term(application(M, N)) :- term(M), term(N).
term(lambda(X, M)) :- v(X), term(M).
term(lambda(M)) :- term(M).
term(parentheses(T)) :- term(T).

:- use_module(utils, [atom_list_concat/2]).

% Convert a user-friendly lambda term into an internally represented lambda term
atom_term(A, T) :- var(A), atoms_term(AS, T), atom_list_concat(AS, A), !.
atom_term(A, T) :- atom_chars(A, CS), atoms_term(CS, T).

atoms_term(AS, T) :- once(phrase(term(T), AS)).

term(T) --> parentheses(T) | lambda(T) | application(T) | variable(T).
parentheses(parentheses(T)) --> ['('], term(T), [')'].
lambda(lambda(X, M)) --> variable(X), ['.', ' '], term(M).
lambda(lambda(M)) --> ['λ', ' '], term(M).
application(application(M, N)) --> parentheses(M), [' '], term(N).
application(application(M, N)) --> lambda(M), [' '], term(N).
application(application(M, N)) --> variable(M), [' '], term(N).
variable(X) --> symbol(S), variable(Y),
  { atom_concat(S, Y, Xi), (atom_number(Xi, X); X = Xi) }.
variable(X) --> var(X), symbol(S), { atom_number(S, X); X = S }.
variable(S) --> symbol(S).
symbol(S) --> [S], { S \= ' ' }.

var(X, Y, Y) :- var(X).

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

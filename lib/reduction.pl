:- module(reduction,
  [ b_reduce/2, b_reducetr/2, substitute/4
  , find_name/2, rename/4, up/3
  , e_reduce/2, e_reducetr/2, free_in/2
  ]).
% Definitions of β-reduction and η-reduction for λ-terms

% What is a β-reduction?
% (x. M) N >> M[x -> N]
b_reduce(abs(X, M), abs(X, N)) :- b_reduce(M, N).
b_reduce(app(abs(X, M), I, N), P) :- I > 1, !,
  (b_reduce(abs(X, M), Q) ->
    P = app(Q, I, N);
    J is I - 1, b_reduce(app(abs(X, M), 1, app(abs(X, M), J, N)), P)).
b_reduce(app(abs(X, M), 1, N), P) :- !, up(N, N1, 1),
  substitute(M, X, N1, P1), up(P1, P, -1).
b_reduce(app(M, I, N), app(P, I, N)) :- b_reduce(M, P), !.
b_reduce(app(M, I, N), app(M, I, Q)) :- b_reduce(N, Q).

b_reducetr(T, V) :- b_reduce(T, U), !, b_reducetr(U, V).
b_reducetr(T, T).

% What is a substitution?
% x[x -> N] := N
% y[x -> N] := y
% (M₁ M₂)[x -> N] := M₁[x -> N] M₂[x -> N]
% (x. M)[y -> N] := x. M[y -> N]
substitute(M, X, N, P) :- substitute(M, X, N, 0, P).
substitute(X-_, X, N, L, P) :- !, up(N, P, L).
substitute(Y-J, X, _, _, Y-J) :- !, X \= Y.
substitute(app(M, I, X-_), X, N, L, app(P, K, Q)) :-
  N = app(M, J, Q), !, K is I + J, substitute(M, X, N, L, P).
substitute(app(M1, I, M2), X, N, L, app(P1, I, P2)) :- !,
  substitute(M1, X, N, L, P1), substitute(M2, X, N, L, P2).
substitute(abs(X, M), X, _, _, abs(X, M)) :- !.
substitute(abs(Y, M), X, N, L, abs(Z, P)) :- X \= Y,
  (free_in(Y, N) -> find_name(Z, N), rename(Y, M, Z, R); R = M, Z = Y),
  % ^ avoid name clashes, e.g.:
  % turn (x. x y)[y -> x] into (a. a y)[y -> x]
  L1 is L + 1, substitute(R, X, N, L1, P).

% Z is a character between a and z that is not free in N
find_name(Z, N) :- between(97, 122, X), char_code(Z, X), not(free_in(Z, N)), !.

% Replace all occurrences of Y-0 in M with Z
rename(Y, M, Z, N) :- rename(Y, M, Z, N, 0).
rename(Y, Y-L, Z, Z-L, L) :- !.
rename(_, X-I, _, X-I, _) :- !.
rename(Y, app(M, I, P), Z, app(N, I, Q), L) :- !,
  rename(Y, M, Z, N, L), rename(Y, P, Z, Q, L).
rename(Y, abs(X, M), Z, abs(U, N), L) :-
  (Y == X -> U = Z; U = X),
  L1 is L + 1, rename(Y, M, Z, N, L1).

% Increase (or decrease) all indices of free variables in M by K
up(M, N, K) :- up(M, N, K, 0).
up(X-I, X-J, K, L) :- I >= L, J is I + K, J >= L, !.
% ^ make sure we don't bind a variable
up(X-I, X-I, _, L) :- I < L.
up(app(M, I, P), app(N, I, Q), K, L) :- up(M, N, K, L), up(P, Q, K, L).
up(abs(X, M), abs(X, N), K, L) :- L1 is L + 1, up(M, N, K, L1).

% What is an η-reduction?
% (x. M x) >> M, if x is not free in M
e_reduce(abs(X, app(M, 1, X-_)), N) :- not(free_in(X, M)), up(M, N, -1), !.
e_reduce(abs(X, M), abs(X, N)) :- e_reduce(M, N).
e_reduce(app(M, I, N), app(P, I, Q)) :- once(e_reduce(M, P); e_reduce(N, Q)).

e_reducetr(T, V) :- e_reduce(T, U), !, e_reducetr(U, V).
e_reducetr(T, T).

free_in(X, M) :- once(free_in(X, M, 0)).
free_in(X, X-I, L) :- I >= L.
free_in(X, app(M, _, N), L) :- free_in(X, M, L); free_in(X, N, L).
free_in(X, abs(Y, M), L) :- X \= Y, L1 is L + 1, free_in(X, M, L1).

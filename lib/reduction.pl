:- module(reduction,
  [ b_reduce/2, substitute/4, up/3, e_reduce/2, free_in/2
  ]).
% Definitions of β-reduction and η-reduction for λ-terms

% What is a substitution?
% x[x -> N] := (N)
% y[x -> N] := y
% (M₁ M₂)[x -> N] := M₁[x -> N] M₂[x -> N]
% (x. M)[y -> N] := x. M[y -> N]
substitute(M, X, N, Mi) :- substitute(M, X, N, 0, Mi).

substitute(X-_, X, N, L, parentheses(Mi)) :- up(N, Mi, L), !.
substitute(Y-J, X, _, _, Y-J) :- X \= Y.
substitute(application(M1, M2), X, N, L, application(M1i, M2i)) :-
  substitute(M1, X, N, L, M1i), substitute(M2, X, N, L, M2i).
substitute(lambda(X, M), X, _, _, lambda(X, M)) :- !.
substitute(lambda(Y, M), X, N, L, lambda(Y, Mi)) :- X \= Y,
  not(free_in(Y-_, N)), Li is L + 1, substitute(M, X, N, Li, Mi).
substitute(parentheses(M), X, N, L, parentheses(Mi)) :-
  substitute(M, X, N, L, Mi).

% Increase (or decrease) all indices of free variables in M by K
up(M, N, K) :- up(M, N, K, 0).
up(X-I, X-Ii, K, L) :- I >= L, Ii is I + K, Ii >= L, !. % make sure we don't bind a variable
up(X-I, X-I, _, L) :- I < L.
up(application(M, N), application(Mi, Ni), K, L) :-
  up(M, Mi, K, L), up(N, Ni, K, L).
up(lambda(X, M), lambda(X, Mi), K, L) :- Li is L + 1, up(M, Mi, K, Li).
up(parentheses(M), parentheses(Mi), K, L) :- up(M, Mi, K, L).

% What is a β-reduction?
% (x. M) N >> M[x -> N]
b_reduce(lambda(X, M), lambda(X, N)) :- b_reduce(M, N).
b_reduce(application(parentheses(M), N), Mi) :-
  b_reduce(application(M, N), Mi), !.
b_reduce(application(M, parentheses(N)), parentheses(Mi)) :-
  b_reduce(application(M, N), Mi), !.
b_reduce(application(lambda(X, M), N), Mii) :- up(N, Ni, 1),
  substitute(M, X, Ni, Mi), up(Mi, Mii, -1), !.
b_reduce(application(M, N), application(Mi, Ni)) :-
  b_reduce(M, Mi); b_reduce(N, Ni).
b_reduce(parentheses(M), parentheses(N)) :- b_reduce(M, N).

% What is an η-reduction?
% (x. M x) >> M, if x is not free in M
e_reduce(lambda(X, application(M, X-_)), M) :- not(free_in(X, M)), !.
e_reduce(lambda(X, M), lambda(X, N)) :- e_reduce(M, N).
e_reduce(application(M, N), application(Mi, Ni)) :- once(e_reduce(M, Mi); e_reduce(N, Ni)).
e_reduce(parentheses(M), parentheses(N)) :- e_reduce(M, N).

free_in(X, M) :- once(free_in(X, M, 0)).
free_in(X, X-I, L) :- I >= L.
free_in(X, application(M, N), L) :- free_in(X, M, L); free_in(X, N, L).
free_in(X, lambda(Y, M), L) :- X \= Y, Li is L + 1, free_in(X, M, Li).
free_in(X, parantheses(T), L) :- free_in(X, T, L).

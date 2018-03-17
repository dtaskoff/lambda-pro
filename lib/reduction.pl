:- module(reduction,
  [ b_reduce/2, substitute/4, up/3, e_reduce/2, free_in/2
  ]).
% Definitions of β-reduction (TODO) and η-reduction for λ-terms

% What is a substitution?
% x[x -> N] := N
% y[x -> N] := y
% (M₁ M₂)[x -> N] := M₁[x -> N] M₂[x -> N]
% (x. M)[y -> N] := x. M[y -> N]
substitute(M, X, N, Mi) :- substitute(M, X, N, 0, Mi).

substitute(X, X, N, L, Ni) :- number(X), up(N, Ni, L), !.
substitute(Y, X, _, _, Y) :- number(X), number(Y), X \= Y.
substitute(application(M1, M2), X, N, L, application(M1i, M2i)) :-
  substitute(M1, X, N, L, M1i), substitute(M2, X, N, L, M2i).
substitute(lambda(M), X, N, L, lambda(Mi)) :- X \= L - 1,
  not(free_in(0, N)), Xi is X + 1, Li is L + 1,
  substitute(M, Xi, N, Li, Mi).
substitute(parentheses(M), X, N, L, parentheses(Mi)) :-
  substitute(M, X, N, L, Mi).

up(X, Xi, K) :- up(X, Xi, K, 0).
up(X, Xi, K, L) :- number(X), X >= L, Xi is X + K, !.
up(X, X, _, L) :- number(X), X < L.
up(application(M, N), application(Mi, Ni), K, L) :-
  up(M, Mi, K, L), up(N, Ni, K, L).
up(lambda(M), lambda(Mi), K, L) :- Li is L + 1, up(M, Mi, K, Li).
up(parentheses(M), parentheses(Mi), K, L) :- up(M, Mi, K, L).

% What is a β-reduction?
% (x. M) N >> M[x -> N]
b_reduce(lambda(M), lambda(N)) :- b_reduce(M, N).
b_reduce(application(lambda(M), parentheses(N)), Mi) :-
  b_reduce(application(lambda(M), N), Mi), !.
b_reduce(application(lambda(M), N), Mii) :- up(N, Ni, 1), substitute(M, 0, Ni, Mi), up(Mi, Mii, -1), !.
b_reduce(application(parentheses(lambda(M)), N), parentheses(Mi)) :- b_reduce(application(lambda(M), N), Mi).
b_reduce(application(M, N), application(Mi, Ni)) :-
  b_reduce(M, Mi); b_reduce(N, Ni).

% What is an η-reduction?
% (x. M x) >> M, if x is not free in M
e_reduce(lambda(application(M, X)), M) :-
  number(X), not(free_in(X, M)), !.
e_reduce(lambda(parentheses(M)), parentheses(N)) :-
  e_reduce(lambda(M), N), !.
e_reduce(lambda(M), lambda(N)) :- e_reduce(M, N).
e_reduce(parentheses(M), parentheses(N)) :-
  e_reduce(M, N).
e_reduce(application(M, N), application(Mi, Ni)) :-
  e_reduce(M, Mi); e_reduce(N, Ni).

free_in(X, M) :- once(free_in(X, M, 0)).
free_in(X, X, L) :- number(X), X >= L.
free_in(X, application(M, N), L) :- X >= L,
  (free_in(X, M, L); free_in(X, N, L)).
free_in(X, lambda(M), L) :- X >= L,
  Li is L + 1, free_in(X, M, Li).
free_in(X, parantheses(T), L) :- free_in(X, T, L).

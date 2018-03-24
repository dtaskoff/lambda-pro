:- module(reduction,
  [ b_reduce/2, b_reducetr/2, substitute/4, up/3
  , e_reduce/2, e_reducetr/2, free_in/2
  ]).
% Definitions of β-reduction and η-reduction for λ-terms

% What is a substitution?
% x[x -> N] := (N)
% y[x -> N] := y
% (M₁ M₂)[x -> N] := M₁[x -> N] M₂[x -> N]
% (x. M)[y -> N] := x. M[y -> N]
substitute(M, X, N, Mi) :- substitute(M, X, N, 0, Mi).

substitute(X-_, X, N, L, parens(Mi)) :- up(N, Mi, L), !.
substitute(Y-J, X, _, _, Y-J) :- X \= Y.
substitute(app(M1, M2), X, N, L, app(M1i, M2i)) :-
  substitute(M1, X, N, L, M1i), substitute(M2, X, N, L, M2i).
substitute(abs(X, M), X, _, _, abs(X, M)) :- !.
substitute(abs(Y, M), X, N, L, abs(Y, Mi)) :- X \= Y,
  not(free_in(Y-_, N)), Li is L + 1, substitute(M, X, N, Li, Mi).
substitute(parens(M), X, N, L, parens(Mi)) :-
  substitute(M, X, N, L, Mi).

% Increase (or decrease) all indices of free variables in M by K
up(M, N, K) :- up(M, N, K, 0).
up(X-I, X-Ii, K, L) :- I >= L, Ii is I + K, Ii >= L, !. % make sure we don't bind a variable
up(X-I, X-I, _, L) :- I < L.
up(app(M, N), app(Mi, Ni), K, L) :-
  up(M, Mi, K, L), up(N, Ni, K, L).
up(abs(X, M), abs(X, Mi), K, L) :- Li is L + 1, up(M, Mi, K, Li).
up(parens(M), parens(Mi), K, L) :- up(M, Mi, K, L).

% What is a β-reduction?
% (x. M) N >> M[x -> N]
b_reduce(abs(X, M), abs(X, N)) :- b_reduce(M, N).
b_reduce(app(parens(M), N), Mi) :-
  b_reduce(app(M, N), Mi), !.
b_reduce(app(M, parens(N)), parens(Mi)) :-
  b_reduce(app(M, N), Mi), !.
b_reduce(app(abs(X, M), N), Mii) :- up(N, Ni, 1),
  substitute(M, X, Ni, Mi), up(Mi, Mii, -1), !.
b_reduce(app(M, N), app(Mi, N)) :- b_reduce(M, Mi), !.
b_reduce(app(M, N), app(M, Ni)) :- b_reduce(N, Ni).
b_reduce(parens(M), parens(N)) :- b_reduce(M, N).

b_reducetr(T, Tii) :- b_reduce(T, Ti), b_reducetr(Ti, Tii), !.
b_reducetr(T, T).

% What is an η-reduction?
% (x. M x) >> M, if x is not free in M
e_reduce(abs(X, app(M, X-_)), N) :- not(free_in(X, M)), up(M, N, -1), !.
e_reduce(abs(X, M), abs(X, N)) :- e_reduce(M, N).
e_reduce(app(M, N), app(Mi, Ni)) :- once(e_reduce(M, Mi); e_reduce(N, Ni)).
e_reduce(parens(M), parens(N)) :- e_reduce(M, N).

e_reducetr(T, Tii) :- e_reduce(T, Ti), e_reducetr(Ti, Tii), !.
e_reducetr(T, T).

free_in(X, M) :- once(free_in(X, M, 0)).
free_in(X, X-I, L) :- I >= L.
free_in(X, app(M, N), L) :- free_in(X, M, L); free_in(X, N, L).
free_in(X, abs(Y, M), L) :- X \= Y, Li is L + 1, free_in(X, M, Li).
free_in(X, parantheses(T), L) :- free_in(X, T, L).

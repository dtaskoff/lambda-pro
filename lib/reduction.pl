:- module(reduction, [e_reduce/2, free_in/2]).
% Definitions of β-reduction (TODO) and η-reduction for λ-terms

% What is a substitution?
% x[x -> N] := N
% y[x -> N] := y
% (M₁ M₂)[x -> N] := M₁[x -> N] M₂[x -> N]
% (x. M)[y -> N] := x. M[y -> N]
%
% What is a β-reduction?
% (x. M) N >> M[x -> N]
%
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
  e_reduce(M, Mi), e_reduce(N, Ni).

free_in(X, M) :- once(free_in(X, M, 0)).
free_in(X, X, L) :- number(X), X >= L.
free_in(X, application(M, N), L) :- X >= L,
  (free_in(X, M, L); free_in(X, N, L)).
free_in(X, lambda(M), L) :- X >= L,
  Li is L + 1, free_in(X, M, Li).
free_in(X, parantheses(T), L) :- free_in(X, T, L).

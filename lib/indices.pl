:- module(indices, [term_de_bruijn/4, index_of/4]).
% Conversions between λ-terms with and without de Bruijn indices

term_de_bruijn(T, N, I, I) :- var(T), use_names(N, T), !.
term_de_bruijn(T, N, I, Ii) :- use_indices(T, N, I, Ii).

name(x). name(y). name(z). name(u). name(v). name(w).
names(Xs) :- bagof(X, name(X), Xs).

% Replace all de Bruijn indices with names
use_names(N, T) :- names(V), empty_assoc(G),
  use_names(N, T, 0, V, _, G, _).

% use_names(TermWithDeBruijnIndices,
%   TermWithNames,
%   NumberOfLambdas,
%   FreeNamesIn,
%   FreeNamesOut,
%   NamesCorrespondingToIndicesIn,
%   NamesCorrespondingToIndicesOut).
% 
% Use a name for an index, which represents a bound variable
use_names(N, X, L, V, V, G, G) :-
  number(N), Ni is N - L, get_assoc(Ni, G, X), !.
% Use a name for an index, which represents a free variable
use_names(N, X, L, [X|V], V, G, Gi) :-
  number(N), Ni is N - L, put_assoc(Ni, G, X, Gi).

use_names(application(MN, NN), application(M, N), L, V, Vii, G, Gii) :-
  use_names(MN, M, L, V, Vi, G, Gi),
  use_names(NN, N, L, Vi, Vii, Gi, Gii).

use_names(lambda(N), lambda(X, M), L, V, Vii, G, Giii) :-
  use_names(-1, X, L, V, Vi, G, Gi),
  Ni is -L - 1, put_assoc(Ni, Gi, X, Gii), Li is L + 1,
  use_names(N, M, Li, Vi, Vii, Gii, Giii).
  
use_names(parentheses(N), parentheses(T), L, V, Vi, G, Gi) :-
  use_names(N, T, L, V, Vi, G, Gi).


% Give all names de Bruijn indices
use_indices(T, N, I, Ii) :- empty_assoc(G),
  use_indices(T, N, 0, I, Ii, G, _).

% use_indices(TermWithNames,
%   TermWithDeBruijnIndices,
%   NumberOfLambdas,
%   NextDeBruijnIndex,
%   NextDeBruijnIndexOut,
%   NamesCorrespondingToIndicesIn,
%   NamesCorrespondingToIndicesOut).
%
% Use an index for a variable, which already has
% a de Bruijn index assigned
use_indices(X, N, L, I, I, G, G) :-
  atom(X), get_assoc(X, G, Ni), N is Ni + L, !.
% Assign a de Bruijn index to a variable
use_indices(X, N, L, I, Ii, G, Gi) :-
  atom(X), N is I + L, put_assoc(X, G, I, Gi), Ii is I - 1.

use_indices(application(M, N), application(MN, NN), L, I, Iii, G, Gii) :-
  use_indices(M, MN, L, I, Ii, G, Gi),
  use_indices(N, NN, L, Ii, Iii, Gi, Gii).

use_indices(lambda(X, M), lambda(N), L, I, Ii, G, Giii) :-
  put_assoc(X, G, -1 - L, Gi),
  use_indices(M, N, L + 1, I, Ii, Gi, Gii),
  del_assoc(X, Gii, -1 - L, Giii).

use_indices(parentheses(T), parentheses(N), L, I, Ii, G, Gi) :-
  use_indices(T, N, L, I, Ii, G, Gi).

% The de Bruijn index of a variable
index_of(X, M, J, I):- term_de_bruijn(M, N, I, _),
  once(index_of(X, M, N, J, 0, true)).

index_of(X, X, N, N, L, true) :- atom(X), number(N), N >= L.
index_of(X, Y, _, _, _, false) :- atom(Y), X \= Y.
index_of(X, application(M, N), application(MN, NN), I, L, _) :-
  index_of(X, M, MN, I, L, true);
  index_of(X, M, MN, I, L, false),
  index_of(X, N, NN, I, L, true).
index_of(X, lambda(X, _), _, _, _, false).
index_of(X, lambda(Y, M), lambda(N), I, L, B) :- X \= Y,
  Li is L + 1, index_of(X, M, N, I, Li, B).
index_of(X, parentheses(M), parentheses(N), I, L, B) :-
  index_of(X, M, N, I, L, B).

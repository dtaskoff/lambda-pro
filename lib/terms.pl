:- module(terms, [term/1, term_to_atom/3, atom_to_term/5, eq/2, free_variables/2]).
% Definitions of λ-terms and conversions between
% λ-terms and prolog atoms

% What is a λ-term?
% <λ-term> ::= <λ-abstraction> | <application> | <variable> | (<λ-term>)
% <λ-abstraction> ::= <variable>. <λ-term> | λ <λ-term>
% <application> ::= <λ-term> <λ-term>
% <variable> ::= s-i where s ∈ Σ⁺ and i ∈ N
%
% Internally, both variable name and index of de Bruijn are stored:
term(X-I) :- atom(X), number(I).
term(app(M, I, N)) :- number(I), I > 0, term(M), term(N).
% ^ an optimisation for iterated functions of this sort:
% f (f (f (f (f x))))
% Note that f could be a much more complicated λ-term
term(abs(X, M)) :- atom(X), term(M).

:- use_module(utils, [atom_list_concat/2]).

% Convert between user-friendly λ-terms and internally represented λ-terms
% term_to_atom(Term, Atom, Type), where
% Type ::= normal | de_bruijn
term_to_atom(T, '...', _) :- '$term_size'(T, _, Sz), Sz > 256, !.
term_to_atom(T, A, Ty) :- nonvar(T), nonvar(Ty), term_to_atom_(T, A, Ty).
term_to_atom_(X-_, X, normal) :- !.
term_to_atom_(_-I, I, de_bruijn) :- !.
term_to_atom_(app(M, I, abs(X, N)), A, Ty) :- !,
  term_to_atom_(app(M, I, par(abs(X, N))), A, Ty).
term_to_atom_(app(M, I, app(N, J, P)), A, Ty) :- !,
  term_to_atom_(app(M, I, par(app(N, J, P))), A, Ty).
term_to_atom_(app(abs(X, M), I, N), A, Ty) :- !,
  term_to_atom_(app(par(abs(X, M)), I, N), A, Ty).
term_to_atom_(app(M, 1, N), A, Ty) :- !,
  term_to_atom_(M, B, Ty), term_to_atom_(N, C, Ty),
  atom_list_concat([B, ' ', C], A).
term_to_atom_(app(M, I, N), A, Ty) :- !, I > 1, J is I - 1,
  term_to_atom_(app(M, 1, par(app(M, J, N))), A, Ty).
term_to_atom_(par(T), A, Ty) :- !,
  term_to_atom_(T, B, Ty), atom_list_concat(['(', B, ')'], A).
term_to_atom_(abs(X, M), A, normal) :- !,
  term_to_atom_(M, MA, normal), atom_list_concat([X, '. ', MA], A).
term_to_atom_(abs(_, M), A, de_bruijn) :-
  term_to_atom_(M, MA, de_bruijn), atom_list_concat(['λ ', MA], A).

% This one has more arguments, because it has to fill the missing variables' names or indices
% atom_to_term(Atom, Term, Type, VariablesIn, VariablesOut), where
% Variables ::= (NextIndex, Names)
atom_to_term(A, T, Ty, V, Vi) :- empty_assoc(B), atom_chars(A, As),
  once(phrase(trm(Ti, Ty, (B, _, V, Vi, 0)), As)), unpar(Ti, T).

unpar(par(T), U) :- unpar(T, U).
unpar(abs(X, M), abs(X, N)) :- unpar(M, N).
unpar(app(M, I, N), app(P, I, J)) :- unpar(M, P), unpar(N, J).
unpar(X-I, X-I).

% A grammar for reading user-friendly λ-terms
% term(Term, Type, State), where
% State ::= (BoundNamesIn, BoundNamesOut, VariablesIn, VariablesOut, LambdasPassed)
trm(T, Ty, S) --> par(T, Ty, S) | lmd(T, Ty, S) | app(T, Ty, S) | vrb(T, Ty, S).
par(par(T), Ty, S) --> ['('], trm(T, Ty, S), [')'].
lmd(abs(X, M), normal, (B, B, I, Ii, L)) --> nam(X), ['.', ' '],
  { J is -1 - L, Li is L + 1, put_assoc(X, B, J, Bi) },
  trm(M, normal, (Bi, _, I, Ii, Li)).
lmd(abs(N, M), de_bruijn, (B, B, [N|Ns], Nsi, L)) --> ['λ', ' '],
  { J is -L, Li is L + 1, put_assoc(J, B, N, Bi) },
  trm(M, de_bruijn, (Bi, _, Ns, Nsi, Li)).
app(App, Ty, (B, Bii, V, Vii, L)) -->
  fnc(M, Ty, (B, Bi, V, Vi, L)), [' '], trm(N, Ty, (Bi, Bii, Vi, Vii, L)),
  { reorder_app(M, N, App) }.
fnc(M, Ty, S) --> par(M, Ty, S) | lmd(M, Ty, S) | vrb(M, Ty, S).
vrb(X-J, normal, (B, Bi, I, Ii, L)) --> nam(X),
  { get_assoc(X, B, K) ->
      J is K + L, Bi = B, Ii = I;
      J is I + L, Ii is I - 1, put_assoc(X, B, J, Bi) }.
vrb(X-J, de_bruijn, (B, Bi, Ns, Nsi, L)) --> idx(J),
  { Ji is J - L + 1, get_assoc(Ji, B, Y) ->
      X = Y, Bi = B, Nsi = Ns;
      Ns = [N|Nsi], X = N, put_assoc(J, B, X, Bi) }.
nam(X) --> sym(S), nam(Y), { atom_concat(S, Y, X) }.
nam(S) --> sym(S).
sym(S) --> [S], { S \= 'λ', S \= ' ', S \= '(', S \= ')', S \= '.' }.
idx(X) --> dgt(D), idx(Y), { atom_concat(D, Y, Xi), atom_number(Xi, X) }.
idx(D) --> dgt(D).
dgt(D) --> [C], { char_type(C, digit(D)) }.

% Reorder applications to match their left associativity
reorder_app(M, par(app(M, I, P)), app(M, J, P)) :- !, J is I + 1.
reorder_app(M, par(N), app(M, 1, par(N))) :- !.
reorder_app(M, app(N, I, P), app(Q, 1, app(N, J, P))) :- I > 1, !,
  J is I - 1, reorder_app(M, N, Q).
reorder_app(M, app(N, 1, P), app(Q, I, R)) :- !, reorder_app(M, N, Q),
  (P = par(app(Q, J, S)) -> I is J + 1, R = S; I is 1, R = P).
reorder_app(M, N, app(M, 1, N)).

% α-equivalence for internally represented λ-terms
eq(_-I, _-I) :- !, number(I).
eq(app(M, I, N), app(P, I, Q)) :- !, eq(M, P), eq(N, Q).
eq(abs(_, M), abs(_, N)) :- eq(M, N).

% The free variables of an internally represented λ-term
free_variables(X-I, [X-I]).
free_variables(app(M, _, N), FV) :-
  free_variables(M, FVM), free_variables(N, FVN), union(FVM, FVN, FV).
free_variables(abs(X, M), FV) :-
  free_variables(M, FVM), subtract(FVM, [X-_], FV).

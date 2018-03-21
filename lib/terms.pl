:- module(terms, [term/1, term_to_atom/3, atom_to_term/5, index_of/3, eq/2, free_variables/2]).
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
term(application(M, N)) :- term(M), term(N).
term(lambda(X, M)) :- atom(X), term(M).
term(parentheses(T)) :- term(T).

:- use_module(utils, [atom_list_concat/2]).

% Convert between user-friendly λ-terms and internally represented λ-terms
% term_to_atom(Term, Atom, Type), where
% Type ::= normal | de_bruijn
term_to_atom(parentheses(T), A, Ty) :- term_to_atom(T, A, Ty), !.
term_to_atom(T, A, Ty) :- nonvar(T), nonvar(Ty), once(term_to_atom_(T, A, Ty)).
term_to_atom_(X-_, X, normal).
term_to_atom_(_-I, I, de_bruijn).
term_to_atom_(parentheses(X-_), X, normal).
term_to_atom_(parentheses(_-I), I, de_bruijn).
term_to_atom_(application(application(M, N), P), A, Ty) :-
  term_to_atom_(application(parentheses(application(M, N)), P), A, Ty).
term_to_atom_(application(lambda(X, M), N), A, Ty) :-
  term_to_atom_(application(parentheses(lambda(X, M)), N), A, Ty).
term_to_atom_(application(M, N), A, Ty) :- term_to_atom_(M, MA, Ty), term_to_atom_(N, NA, Ty),
  atom_list_concat([MA, ' ', NA], A).
term_to_atom_(lambda(X, M), A, normal) :- term_to_atom_(M, MA, normal),
  atom_list_concat([X, '. ', MA], A).
term_to_atom_(lambda(_, M), A, de_bruijn) :- term_to_atom_(M, MA, de_bruijn),
  atom_list_concat(['λ ', MA], A).
term_to_atom_(parentheses(parentheses(T)), A, Ty) :- term_to_atom_(parentheses(T), A, Ty).
term_to_atom_(parentheses(T), A, Ty) :- term_to_atom_(T, TA, Ty), atom_list_concat(['(', TA, ')'], A).

% This one has more arguments, because it has to fill the missing variables' names or indices
% atom_to_term(Atom, Term, Type, VariablesIn, VariablesOut), where
% Variables ::= (NextIndex, Names)
atom_to_term(A, T, Ty, V, Vi) :- empty_assoc(B), atom_chars(A, As), once(phrase(trm(T, Ty, (B, _, V, Vi, 0)), As)).

% A grammar for reading user-friendly λ-terms
% term(Term, Type, State), where
% State ::= (BoundNamesIn, BoundNamesOut, VariablesIn, VariablesOut, LambdasPassed)
trm(T, Ty, S) --> par(T, Ty, S) | lmd(T, Ty, S) | app(T, Ty, S) | vrb(T, Ty, S).
par(parentheses(T), Ty, S) --> ['('], trm(T, Ty, S), [')'].
lmd(lambda(X, M), normal, (B, B, I, Ii, L)) --> nam(X),
  { J is -1 - L, Li is L + 1, put_assoc(X, B, J, Bi) },
  ['.', ' '], trm(M, normal, (Bi, _, I, Ii, Li)).
lmd(lambda(N, M), de_bruijn, (B, B, [N|Ns], Nsi, L)) --> ['λ', ' '],
  { J is -L, Li is L + 1, put_assoc(J, B, N, Bi) },
  trm(M, de_bruijn, (Bi, _, Ns, Nsi, Li)).
app(application(M, N), Ty, (B, Bii, V, Vii, L)) -->
  fnc(M, Ty, (B, Bi, V, Vi, L)), [' '], trm(N, Ty, (Bi, Bii, Vi, Vii, L)).
fnc(M, Ty, S) --> par(M, Ty, S) | lmd(M, Ty, S) | vrb(M, Ty, S).
vrb(X-J, normal, (B, Bi, I, Ii, L)) --> nam(X),
  { get_assoc(X, B, K) -> J is K + L, Bi = B, Ii = I; J is I + L, Ii is I - 1, put_assoc(X, B, J, Bi) }.
vrb(X-J, de_bruijn, (B, Bi, Ns, Nsi, L)) --> idx(J),
  { Ji is J - L + 1, get_assoc(Ji, B, Y) -> X = Y, Bi = B, Nsi = Ns; Ns = [N|Nsi], X = N, put_assoc(J, B, X, Bi) }.
nam(X) --> sym(S), nam(Y), { atom_concat(S, Y, X) }.
nam(S) --> sym(S).
sym(S) --> [S], { S \= 'λ', S \= ' ', S \= '(', S \= ')', S \= '.' }.
idx(X) --> dgt(D), idx(Y), { atom_concat(D, Y, Xi), atom_number(Xi, X) }.
idx(D) --> dgt(D).
dgt(D) --> [C], { char_type(C, digit(D)) }.

% The de Bruijn index of a variable
index_of(X, X-I, I).
index_of(X, application(M, N), I) :- once(index_of(X, M, I); index_of(X, N, I)).
index_of(X, lambda(Y, M), I) :- X \= Y, index_of(X, M, I), !.
index_of(X, parentheses(M), I) :- index_of(X, M, I).

% α-equivalence for internally represented λ-terms
eq(_-I, _-I) :- number(I), !.
eq(application(M, N), application(Mi, Ni)) :- eq(M, Mi), eq(N, Ni), !.
eq(lambda(_, M), lambda(_, N)) :- eq(M, N), !.
eq(parentheses(M), N) :- eq(M, N), !.
eq(M, parentheses(N)) :- eq(M, N).

% The free variables of an internally represented λ-term
free_variables(X-I, [X-I]).
free_variables(application(M, N), FV) :-
  free_variables(M, FVM), free_variables(N, FVN), union(FVM, FVN, FV).
free_variables(lambda(X, M), FV) :-
  free_variables(M, FVM), subtract(FVM, [X-_], FV).
free_variables(parentheses(T), FV) :- free_variables(T, FV).

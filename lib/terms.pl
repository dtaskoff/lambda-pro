:- module(terms, [term/1, atom_term/2]).
% Definitions of λ-terms and conversions between
% λ-terms and prolog atoms

% What is a λ-term?
% <λ-term> ::= <λ-abstraction> | <application> | <variable> | (<λ-term>)
% <λ-abstraction> ::= <variable>. <λ-term> | λ <λ-term>
% <application> ::= <λ-term> <λ-term>
% <variable> ::= s where s ∈ Σ⁺ | n where n ∈ N
term(X) :- atom(X), !.
term(X) :- number(X).
term(application(M, N)) :- term(M), term(N).
term(lambda(X, M)) :- atom(X), term(M).
term(lambda(M)) :- term(M).
term(parentheses(T)) :- term(T).

:- use_module(utils, [atom_list_concat/2]).

% Convert a user-friendly λ-term into an internally represented λ-term
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
variable(X) --> index(X), ! | name(X).

index(X) --> var(X), digit(D), index(Y),
  { atom_concat(D, Y, Xi), atom_number(Xi, X) }.
index(N) --> [A], { number(N), atom_number(A, N) }.
index(D) --> digit(D).
digit(Di) --> var(Di), [D], { char_type(D, digit(Di)) }.

name(X) --> symbol(S), name(Y), { atom_concat(S, Y, X) }.
name(S) --> symbol(S).
symbol(S) --> [S],
  { S \= 'λ', S \= ' ', S \= '(', S \= ')', S \= '.' }.

var(X, Y, Y) :- var(X).

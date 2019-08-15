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
% Variables ::= if Type is 'normal', then next index,
% else (type is 'de_bruijn'), a list of variables' names to fill
atom_to_term(A, T, Ty, Vin, Vout) :- empty_assoc(B), atom_chars(A, As),
  once(phrase(trm(TP, Ty, (B, _, Vin, Vout, 0)), As)), unpar(TP, T).

unpar(par(T), U) :- unpar(T, U).
unpar(abs(X, M), abs(X, N)) :- unpar(M, N).
unpar(app(M, I, N), app(P, I, J)) :- unpar(M, P), unpar(N, J).
unpar(X-I, X-I).

% A grammar for reading user-friendly λ-terms
% term(Term, Type, State), where
% State ::= (BoundNamesIn, BoundNamesOut, VariablesIn, VariablesOut, LambdasPassed)
% a term
trm(T, Ty, S) --> par(T, Ty, S) | lmd(T, Ty, S) | app(T, Ty, S) | vrb(T, Ty, S).
% a term in parens
par(par(T), Ty, S) --> ['('], trm(T, Ty, S), [')'].
% a lambda term
lmd(abs(X, M), normal, (B, B, Iin, Iout, L)) --> nam(X), ['.', ' '],
  { J is -1 - L, L1 is L + 1, put_assoc(X, B, J, B1) },
  trm(M, normal, (B1, _, Iin, Iout, L1)).
lmd(abs(N, M), de_bruijn, (B, B, [N|Ns], Ns1, L)) --> ['λ', ' '],
  { J is -L, L1 is L + 1, put_assoc(J, B, N, B1) },
  trm(M, de_bruijn, (B1, _, Ns, Ns1, L1)).
% an application
app(App, Ty, (B, B2, V, V2, L)) -->
  fnc(M, Ty, (B, B1, V, V1, L)), [' '], trm(N, Ty, (B1, B2, V1, V2, L)),
  { reorder_app(M, N, App) }.
% a function (the left part of an application)
fnc(M, Ty, S) --> par(M, Ty, S) | lmd(M, Ty, S) | vrb(M, Ty, S).
% a variable
vrb(X-J, normal, (B, B1, I, I1, L)) --> nam(X),
  { get_assoc(X, B, K) ->
      J is K + L, B1 = B, I1 = I;
      J is I + L, I1 is I - 1, put_assoc(X, B, J, B1) }.
vrb(X-J, de_bruijn, (B, B1, Ns, Ns1, L)) --> idx(J),
  { J1 is J - L + 1, get_assoc(J1, B, Y) ->
      X = Y, B1 = B, Ns1 = Ns;
      Ns = [N|Ns1], X = N, put_assoc(J, B, X, B1) }.
% a variable name
nam(X) --> sym(S), nam(Y), { atom_concat(S, Y, X) }.
nam(S) --> sym(S).
% a symbol (part of a variable name)
sym(S) --> [S], { S \= 'λ', S \= ' ', S \= '(', S \= ')', S \= '.' }.
% an index (of de Bruijn)
idx(X) --> dgt(D), idx(Y), { atom_concat(D, Y, X1), atom_number(X1, X) }.
idx(D) --> dgt(D).
% a digit (part of an index)
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
% Note: counting on the fact, that applications are normalised, and parens are removed (see unpar)
% (e.g. f (f x) can be represented as 'app(f-_, 2, x-_)', and as 'app(f-_, 1, app(f-_, 1, x-_)')
eq(_-I, _-I) :- !, number(I).
eq(app(M, I, N), app(P, I, Q)) :- !, eq(M, P), eq(N, Q).
eq(abs(_, M), abs(_, N)) :- eq(M, N).

% The free variables of an internally represented λ-term
free_variables(X-I, [X-I]).
free_variables(app(M, _, N), FV) :-
  free_variables(M, FVM), free_variables(N, FVN), union(FVM, FVN, FV).
free_variables(abs(X, M), FV) :-
  free_variables(M, FVM), subtract(FVM, [X-_], FV).

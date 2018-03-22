:- module(interpret, [interpret/3]).


% Interpret a λ-term given an associative list of 'meanings'
% for its variables
% interpret(Term, Meanings, Interpretation)
interpret(X-_, Ms, I) :- get_assoc(X, Ms, I).
interpret(application(M, N), Ms, I) :-
  interpret(M, Ms, X), interpret(N, Ms, Y), call(X, Y, I).
interpret(lambda(X, M), Ms, [Y, I]>>(I = J)) :-
  put_assoc(X, Ms, Y, Msi),
  interpret(M, Msi, J).
interpret(parentheses(T), Ms, I) :- interpret(T, Ms, I).

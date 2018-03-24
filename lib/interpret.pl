:- module(interpret, [interpret/3]).


% Interpret a λ-term given an associative list of 'meanings'
% for its variables
% interpret(Term, Meanings, Interpretation)
interpret(X-_, Ms, I) :- get_assoc(X, Ms, I).
interpret(app(M, N), Ms, I) :-
  interpret(M, Ms, X), interpret(N, Ms, Y), call(X, Y, I).
interpret(abs(X, M), Ms, [Y, I]>>(I = J)) :-
  put_assoc(X, Ms, Y, Msi),
  interpret(M, Msi, J).
interpret(parens(T), Ms, I) :- interpret(T, Ms, I).

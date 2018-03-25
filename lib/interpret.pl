:- module(interpret, [interpret/3]).


% Interpret a λ-term given an associative list of 'meanings'
% for its variables
% interpret(Term, Meanings, Interpretation)
interpret(X-_, Ms, I) :- get_assoc(X, Ms, I).
interpret(app(M, J, N), Ms, I) :-
  interpret(M, Ms, F), interpret(N, Ms, X), iterate(F, J, X, I).
interpret(abs(X, M), Ms, [Y, I]>>(I = J)) :-
  put_assoc(X, Ms, Y, Msi),
  interpret(M, Msi, J).
interpret(parens(T), Ms, I) :- interpret(T, Ms, I).

iterate(F, N, X, I) :- N > 0, !, call(F, X, Y), M is N - 1,
  iterate(F, M, Y, I).
iterate(_, 0, X, X).

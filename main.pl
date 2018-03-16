% What is a lambda term?
term(X) :- v(X).                             % a variable
term(application(M, N)) :- term(M), term(N). % an application
term(lambda(X, M)) :- v(X), term(M).         % a lambda abstraction

% The set of allowed variables
v(u). v(v). v(w). v(x). v(y). v(z).

% Lambda terms in a user-friendly format
% term(x) -> x
% term(application(M, N)) -> (M N)
% term(lambda(x, M)) -> x. M

% Parse a user-friendly lambda term
% into a lambda term defined in prolog
parse(S, T) :- string_chars(S, CS), once(phrase(term(T), CS)).

term(T) --> lambda(T) | application(T) | variable(T) | ['('], term(T), [')'].
lambda(lambda(X, M)) --> variable(X), ['.', ' '], term(M).
application(application(M, N)) --> lambda(M), [' '], term(N).
application(application(M, N)) --> variable(M), [' '], term(N).
application(application(M, N)) --> ['('], term(M), [')', ' '], term(N).
variable(X) --> [C], { atom_string(X, C), v(X) }.

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
parse(S) :- string_chars(S, CS), once(phrase(term, CS)).

term        --> lambda | application | variable | ['('], term, [')'].
lambda      --> variable, ['.', ' '], term.
application --> lambda, [' '], term.
application --> variable, [' '], term.
application --> ['('], term, [')', ' '], term.
variable    --> [C], { atom_string(X, C), v(X) }.

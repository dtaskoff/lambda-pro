% What is a lambda term?
term(X) :- v(X).                             % a variable
term(application(M, N)) :- term(M), term(N). % an application
term(lambda(X, M)) :- v(X), term(M).         % a lambda abstraction

% The set of allowed variables
v(u). v(v). v(w). v(x). v(y). v(z).

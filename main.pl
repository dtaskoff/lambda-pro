% What is a lambda term?
term(X) :- v(X).                             % a variable
term(application(M, N)) :- term(M), term(N). % an application
term(lambda(X, M)) :- v(X), term(M).         % a lambda abstraction
term(parentheses(T)) :- term(T).

:- use_module(utils, [atom_list_concat/2]).

show_term(X, X) :- v(X).
show_term(application(M, N), S) :- show_term(M, MS), show_term(N, NS),
  atom_list_concat([MS, ' ', NS], S).
show_term(lambda(X, M), S) :- show_term(M, MS),
  atom_list_concat([X, '. ', MS], S).
show_term(parentheses(T), S) :- show_term(T, TS),
  atom_list_concat(['(', TS, ')'], S).

% The set of allowed variables
v(u). v(v). v(w). v(x). v(y). v(z).

% Lambda terms in a user-friendly format
% term(x) -> x
% term(application(M, N)) -> M N
% term(lambda(x, M)) -> x. M
% term(parentheses(T)) -> (T)

% Parse a user-friendly lambda term
% into a lambda term defined in prolog
parse(S, T) :- string_chars(S, CS), once(phrase(term(T), CS)).

term(T) --> lambda(T) | application(T) | variable(T) | parentheses(T).
lambda(lambda(X, M)) --> variable(X), ['.', ' '], term(M).
application(application(M, N)) --> lambda(M), [' '], term(N).
application(application(M, N)) --> variable(M), [' '], term(N).
application(application(M, N)) --> parentheses(M), [' '], term(N).
variable(X) --> [C], { atom_string(X, C), v(X) }.
parentheses(parentheses(T)) --> ['('], term(T), [')'].

% What is a 'de Bruijn' term?
de_bruijn_term(X) :- number(X).
de_bruijn_term(application(M, N)) :- de_bruijn_term(M), de_bruijn_term(N).
de_bruijn_term(lambda(M)) :- de_bruijn_term(M).
de_bruijn_term(parentheses(T)) :- de_bruijn_term(T).

show_de_bruijn_term(X, X) :- number(X).
show_de_bruijn_term(application(M, N), S) :- show_de_bruijn_term(M, MS), show_de_bruijn_term(N, NS),
  atom_list_concat([MS, ' ', NS], S).
show_de_bruijn_term(lambda(M), S) :- show_de_bruijn_term(M, MS),
  atom_list_concat(['λ ', MS], S).
show_de_bruijn_term(parentheses(T), S) :- show_de_bruijn_term(T, TS),
  atom_list_concat(['(', TS, ')'], S).


% Call the helper function with initialised accumulators
to_de_bruijn(X, N) :- empty_assoc(G), to_de_bruijn(X, N, 0, 42, _, G, _).
% Convert a lambda term into a de Bruijn term:
%
% convert a variable, which already has a de Bruijn index
to_de_bruijn(X, N, L, K, K, G, G) :- v(X), get_assoc(X, G, Ni), N is Ni + L, !.
% convert a variable, which doesn't have a de Bruijn index yet
to_de_bruijn(X, N, L, K, Ki, G, Gi) :- v(X), N is K + L, put_assoc(X, G, K, Gi), Ki is K - 1.

to_de_bruijn(application(M, N), application(MN, NN), L, K, Kii, G, Gii) :-
  to_de_bruijn(M, MN, L, K, Ki, G, Gi), to_de_bruijn(N, NN, L, Ki, Kii, Gi, Gii).

to_de_bruijn(lambda(X, M), lambda(MN), L, K, Ki, G, Giii) :-
  put_assoc(X, G, -1 - L, Gi), to_de_bruijn(M, MN, L + 1, K, Ki, Gi, Gii), del_assoc(X, Gii, -1 - L, Giii).
to_de_bruijn(parentheses(T), parentheses(TN), L, K, Ki, G, Gi) :-
  to_de_bruijn(T, TN, L, K, Ki, G, Gi).

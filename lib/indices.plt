:- begin_tests(indices).
:- use_module(indices, [term_de_bruijn/4, index_of/4]).
:- use_module(test_terms).

test(term_de_bruijn,
  forall((term(X, name, lambda, T),
          term(X, de_bruijn, lambda, N)))) :-
  term_de_bruijn(T, N, 42, _).

test(index_of) :-
  not(index_of(x, lambda(x, application(y, x)), _, 42)).

test(index_of) :-
  index_of(y, lambda(x, application(y, x)), 43, 42).

:- end_tests(indices).

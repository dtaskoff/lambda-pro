:- begin_tests(indices).
:- use_module(indices, [term_de_bruijn/2]).
:- use_module(test_terms).

test(term_de_bruijn,
  forall((term(X, name, lambda, T),
          term(X, de_bruijn, lambda, N)))) :-
  term_de_bruijn(T, N).

:- end_tests(indices).

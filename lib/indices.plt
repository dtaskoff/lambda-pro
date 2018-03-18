:- begin_tests(indices).
:- use_module(indices, [term_de_bruijn/2, max_index/1, index_of/3]).
:- use_module(test_terms).

test(term_de_bruijn,
  forall((term(X, name, lambda, T),
          term(X, de_bruijn, lambda, N)))) :-
  term_de_bruijn(T, N).

test(index_of) :-
  not(index_of(x, lambda(x, application(y, x)), _)).

test(index_of) :-
  max_index(I), Ix is I + 1,
  index_of(y, lambda(x, application(y, x)), Ix).

:- end_tests(indices).

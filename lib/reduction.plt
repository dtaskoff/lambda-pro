:- begin_tests(reduction).
:- use_module(reduction, [free_in/2, e_reduce/2]).
:- use_module(test_terms).

test(free_in,
  forall((member(X, [0, 1, 2]),
          term(_, de_bruijn, lambda, T)))) :-
  not(free_in(X, T)).

test(free_in, [setup(term(42, de_bruijn, lambda, T))]) :-
  free_in(43, T).

test(e_reduce,
  forall((term(C, de_bruijn, lambda, T), C \= c1)) :-
  not(e_reduce(T, _)).

test(e_reduce) :-
  e_reduce(lambda(application(lambda(0), 1)), lambda(0)).

:- end_tests(reduction).

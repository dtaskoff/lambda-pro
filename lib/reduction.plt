:- begin_tests(reduction).
:- use_module(reduction,
  [ free_in/2, e_reduce/2
  , up/3, substitute/4, b_reduce/2
  ]).
:- use_module(test_terms).

test(free_in,
  forall((member(X, [0, 1, 2]),
          term(_, de_bruijn, lambda, T)))) :-
  not(free_in(X, T)).

test(free_in, [setup(term(42, de_bruijn, lambda, T))]) :-
  free_in(43, T).

test(e_reduce,
  forall((term(C, de_bruijn, lambda, T), C \= c1))) :-
  not(e_reduce(T, _)).

test(e_reduce) :-
  e_reduce(lambda(application(lambda(0), 1)), lambda(0)).

test(up) :- up(42, 43, 1).
test(up) :- up(43, 42, -1).
test(up) :- up(lambda(lambda(lambda(2))),
  lambda(lambda(lambda(2))), _).
test(up) :- up(lambda(lambda(lambda(3))),
  lambda(lambda(lambda(4))), 1).

test(substitute) :- substitute(42, 42, lambda(42), parentheses(lambda(42))).
test(substitute) :- substitute(42, 41, _, 42).

test(substitute) :-
  substitute(application(0, 1), 0, lambda(42),
    application(parentheses(lambda(42)), 1)).

test(substitute) :-
  substitute(lambda(0), 0, lambda(42), lambda(0)).
test(substitute) :-
  substitute(lambda(1), 0, lambda(42), lambda(parentheses(lambda(43)))).
test(substitute) :-
  not(substitute(lambda(1), 0, lambda(42), lambda(lambda(42)))).
test(substitute) :-
  substitute(lambda(0), 1, lambda(42), lambda(0)).
test(substitute) :-
  substitute(lambda(0), 1, lambda(42), lambda(0)).
test(substitute) :-
  substitute(lambda(application(2, 0)), 1,
    application(2, lambda(lambda(application(application(0, 1), 2)))),
    lambda(application(parentheses(
      application(3, lambda(lambda(application(application(0, 1), 3))))), 0))).

test(b_reduce,
  forall(term(_, de_bruijn, lambda, T))) :-
  not(b_reduce(T, _)).

test(b_reduce) :- b_reduce(application(lambda(0), 42), 42).
test(b_reduce) :- b_reduce(application(lambda(2), 42), 1).
test(b_reduce) :- b_reduce(
  application(lambda(lambda(application(42, 1))),
    lambda(application(0, 1))),
  lambda(application(41, parentheses(lambda(application(0, 2)))))).

:- end_tests(reduction).

:- begin_tests(reduction).
:- use_module(reduction,
  [ free_in/2, e_reduce/2
  , up/3, substitute/4, b_reduce/2
  ]).
:- use_module(terms, [eq/2]).
:- use_module(test_terms).

test(free_in,
  forall((member(X, [x, y, z]),
          term(N, _, term, T), N \= 42))) :-
  not(free_in(X, T)).

test(free_in, [setup(term(42, _, term, T))]) :- free_in(y, T).

test(e_reduce,
  forall((term(C, de_bruijn, term, T), C \= c1))) :-
  not(e_reduce(T, _)).

test(e_reduce) :- T =
  lambda(X, application(lambda(Y, Y-0), X)),
  Ti = lambda(Y, Y-0), e_reduce(T, Ti).

test(up) :- up(X-42, X-43, 1).
test(up) :- up(X-43, X-42, -1).
test(up) :- up(lambda(X, lambda(Y, lambda(Z, Z-2))),
  lambda(X, lambda(Y, lambda(Z, X-2))), _).
test(up) :- up(lambda(X, lambda(Y, lambda(U, V-3))), lambda(X, lambda(Y, lambda(U, V-4))), 1).

test(substitute) :- substitute(x-42, y, _, x-42).

test(substitute) :- M = application(x-42, y-41),
  N = lambda(y, z-41), substitute(M, x, N, Ti),
  Ti = application(parentheses(lambda(y, z-41)), y-41).

test(substitute) :-
  substitute(lambda(x, x-0), x, lambda(x, y-42), lambda(x, x-0)).
test(substitute) :-
  M = lambda(x, y-1), N = lambda(u, v-42),
  substitute(M, y, N, lambda(x, parentheses(lambda(u, v-43)))).

test(substitute) :-
  substitute(lambda(x, x-0), y, lambda(u, v-42), lambda(x, x-0)).
test(substitute) :-
  M = lambda(x, application(y-2, x-0)),
  N = application(z-2, lambda(u, lambda(v, application(application(v-0, u-1), w-2)))),
  substitute(M, y, N, T),
  T = lambda(x, application(parentheses(application(z-3, lambda(u, lambda(v, application(application(v-0, u-1), w-3))))), x-0)).

test(b_reduce,
  forall(term(_, de_bruijn, lambda, T))) :-
  not(b_reduce(T, _)).

test(b_reduce) :-
  M = application(lambda(x, x-0), y-42),
  b_reduce(M, parentheses(y-42)).
test(b_reduce) :-
  M = application(lambda(x, y-2), z-42), b_reduce(M, y-1).
test(b_reduce) :-
  M = application(lambda(x, lambda(y, application(z-42, x-1))),
    lambda(x, application(x-0, u-1))),
  N = lambda(y, application(z-41, parentheses(lambda(x, application(x-0, u-2))))),
  b_reduce(M, N).
test(b_reduce) :- M = lambda(x, application(x-0, x-0)),
  N = application(M, M),
  b_reduce(N, application(parentheses(M), parentheses(M))).

:- end_tests(reduction).

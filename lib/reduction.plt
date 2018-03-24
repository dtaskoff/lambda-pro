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
  abs(X, app(abs(Y, Y-0), X)),
  Ti = abs(Y, Y-0), e_reduce(T, Ti).

test(up) :- up(X-42, X-43, 1).
test(up) :- up(X-43, X-42, -1).
test(up) :- up(abs(X, abs(Y, abs(Z, Z-2))),
  abs(X, abs(Y, abs(Z, X-2))), _).
test(up) :- up(abs(X, abs(Y, abs(U, V-3))), abs(X, abs(Y, abs(U, V-4))), 1).

test(substitute) :- substitute(x-42, y, _, x-42).

test(substitute) :- M = app(x-42, y-41),
  N = abs(y, z-41), substitute(M, x, N, Ti),
  Ti = app(abs(y, z-41), y-41).

test(substitute) :-
  substitute(abs(x, x-0), x, abs(x, y-42), abs(x, x-0)).
test(substitute) :-
  M = abs(x, y-1), N = abs(u, v-42),
  substitute(M, y, N, abs(x, abs(u, v-43))).

test(substitute) :-
  substitute(abs(x, x-0), y, abs(u, v-42), abs(x, x-0)).
test(substitute) :-
  M = abs(x, app(y-2, x-0)),
  N = app(z-2, abs(u, abs(v, app(app(v-0, u-1), w-2)))),
  substitute(M, y, N, T),
  T = abs(x, app(app(z-3, abs(u, abs(v, app(app(v-0, u-1), w-3)))), x-0)).

test(b_reduce,
  forall(term(_, de_bruijn, lambda, T))) :-
  not(b_reduce(T, _)).

test(b_reduce) :-
  M = app(abs(x, x-0), y-42),
  b_reduce(M, y-42).
test(b_reduce) :-
  M = app(abs(x, y-2), z-42), b_reduce(M, y-1).
test(b_reduce) :-
  M = app(abs(x, abs(y, app(z-42, x-1))),
    abs(x, app(x-0, u-1))),
  N = abs(y, app(z-41, abs(x, app(x-0, u-2)))),
  b_reduce(M, N).
test(b_reduce) :- M = abs(x, app(x-0, x-0)),
  N = app(M, M),
  b_reduce(N, app(M, M)).

:- end_tests(reduction).

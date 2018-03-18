:- begin_tests(terms).
:- use_module(terms, [term/1, atom_term/2, free_variables/2]).
:- use_module(test_terms).

test(term, forall(term(_, _, lambda, T))) :- term(T).

test(atom_term,
  forall((term(X, Y, atom, XA),
          term(X, Y, lambda, XL)))) :-
  atom_term(XA, XL).

test(free_variables) :- free_variables(x, [x]).
test(free_variables) :-
  free_variables(application(x, y), [x, y]).
test(free_variables) :-
  free_variables(lambda(x, x), []).
test(free_variables) :-
  free_variables(application(lambda(z, lambda(x, y)),
    lambda(y, lambda(z, x))), [y, x]).
test(free_variables) :- free_variables(x, [x]).

:- end_tests(terms).

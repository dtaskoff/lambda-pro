:- begin_tests(terms).
:- use_module(terms, [term/1, atom_term/2]).
:- use_module(test_terms).

test(term, forall(term(_, _, lambda, T))) :- term(T).

test(atom_term,
  forall((term(X, Y, atom, XA),
          term(X, Y, lambda, XL)))) :-
  atom_term(XA, XL).

:- end_tests(terms).

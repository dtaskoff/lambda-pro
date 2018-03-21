:- begin_tests(terms).
:- use_module(terms, [term/1, term_to_atom/3, atom_to_term/5, index_of/3, free_variables/2]).
:- use_module(test_terms).

test(term, forall(term(_, _, lambda, T))) :- term(T).

test(term_to_atom,
  forall((term(X, Ty, term, T),
          term(X, Ty, atom, A)))) :-
  term_to_atom(T, A, Ty).

test(atom_to_term,
  forall((term(X, Ty, atom, A),
          term(X, Ty, term, T)))) :-
  (Ty = normal -> V = 42; V = [x, y, z]),
  atom_to_term(A, T, Ty, V, _).

test(index_of) :- index_of(y, lambda(x, application(y-43, x-0)), 43).

test(free_variables) :- atom_to_term('(z. x. y) (y. z. x)', T, normal, 42, _), free_variables(T, [y-_, x-_]).
test(free_variables) :- atom_to_term('(λ λ 44) (λ λ 43)', T, de_bruijn, [x, y, z, u, v, w], _), free_variables(T, [_-44, _-43]).

:- end_tests(terms).

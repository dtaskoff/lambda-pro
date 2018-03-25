:- begin_tests(evaluate).
:- use_module(evaluate).
:- use_module(test_terms).
:- use_module(terms).
:- use_module(utils).

test(evaluate_bad_input,
  forall(member(X, [one, two, three]))) :-
    atom_concat('Bad input: ', X, O),
    evaluate:evaluate_bad_input(X, O, S, S, F, F).

test(evaluate_,
  forall((term(X, normal, atom, A),
          term(X, normal, term, T)))) :-
    empty_assoc(Bs),
    (X == 42 ->
      evaluate:evaluate_(A, _, Out, (Bs, [f0], 42), (Bsi, [], 41));
      evaluate:evaluate_(A, _, Out, (Bs, [f0], I), (Bsi, [],  I))),
    evaluate:show_term(f0, A, Out),
    put_assoc(f0, Bs, T, Bsi).

test(evaluate_term,
  forall((term(X, name, atom, A),
          term(X, name, lambda, T)))) :-
    list_to_assoc([f0-T], Bs), S = (Bs, _, _),
    evaluate:evaluate_term(f0, Out, S, S, F, F),
    evaluate:show_term(f0, A, Out).

test(evaluate_substitution,
  forall(term(_, name, lambda, T))) :-
    list_to_assoc([f0-T], Bs),
    evaluate:evaluate_substitution('f0 f0 (x. y)', M, _, Bs, 42, I),
    Ii is I + 2,
    Mi = app(T, 1, app(T, 1, abs(x, y-Ii))),
    eq(M, Mi).

test(evaluate_substitutions,
  forall(term(_, name, lambda, T))) :-
    list_to_assoc([f0-T, f1-T, f2-T], Bs),
    evaluate:evaluate_substitutions('f0 f1 f2', M, _, Bs, 42, _),
    Mi = app(T, 1, app(T, 1, T)),
    eq(M, Mi).

test(x_reduction_beta) :-
  evaluate:x_reduction(b_reduce, 'beta f0 f0', 'f0 f0').
test(x_reduction_eta) :-
  evaluate:x_reduction(e_reduce, 'eta f0', 'f0').

test(evaluate_beta_reduction) :-
  N = abs(x, app(x-0, 1, x-0)),
  Ni = app(N, 1, N),
  list_to_assoc([f0-N], Bs),
  evaluate:evaluate_reduction(
    'beta f0 f0', Out, (Bs, [f1], 42), (Bsi, [], 42), F, F),
  get_assoc(f1, Bsi, Nii), eq(Ni, Nii),
  W = '(x. x x) x. x x',
  atom_list_concat([' -β> \nf1 = ', W], Out).

test(evaluate_eta_reduction) :-
  T = abs(x, app(y-43, 1, x-0)),
  list_to_assoc([f0-T], Bs),
  evaluate:evaluate_reduction(
    'eta f0', Out, (Bs, [f1], 42), (Bsi, [], 42), F, F),
  get_assoc(f1, Bsi, Ti), eq(Ti, y-42),
  atom_list_concat([' -η> \nf1 = y'], Out).

:- end_tests(evaluate).

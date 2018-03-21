:- begin_tests(evaluate).
:- use_module(evaluate).
:- use_module(test_terms).
:- use_module(terms).
:- use_module(utils).

test(evaluate_bad_input,
  forall(member(X, [one, two, three]))) :-
    atom_concat('Bad input: ', X, O),
    evaluate:evaluate_bad_input(X, O).

test(evaluate_,
  forall((term(X, normal, atom, A),
          term(X, normal, term, T),
          term(X, de_bruijn, atom, Ai)))) :-
    empty_assoc(Bs),
    (X == 42 ->
      evaluate:evaluate_(A, Out, Bs, Bsi, [f0], [], 42, 41);
      evaluate:evaluate_(A, Out, Bs, Bsi, [f0], [], I, I)),
    evaluate:show_terms(f0, A, Ai, Out),
    put_assoc(f0, Bs, T, Bsi).

test(evaluate_lambda,
  forall((term(X, name, atom, A),
          term(X, name, lambda, T),
          term(X, de_bruijn, atom, Ai)))) :-
    list_to_assoc([f0-T], Bs),
    evaluate:evaluate_lambda(f0, Out, Bs, Bs, Ns, Ns, I, I),
    evaluate:show_terms(f0, A, Ai, Out).

test(evaluate_substitution,
  forall(term(_, name, lambda, T))) :-
    list_to_assoc([f0-T], Bs),
    evaluate:evaluate_substitution('f0 f0 (x. y)', M, _, Bs, 42, I),
    Ii is I + 2,
    Mi = application(T, application(T, lambda(x, y-Ii))),
    eq(M, Mi).

test(evaluate_substitutions,
  forall(term(_, name, lambda, T))) :-
    list_to_assoc([f0-T, f1-T, f2-T], Bs),
    evaluate:evaluate_substitutions('f0 f1 f2', M, _, Bs, 42, _),
    Mi = application(T, application(T, T)),
    eq(M, Mi).

test(x_reduction_beta) :-
  evaluate:x_reduction(b_reduce, 'beta f0 f0', 'f0 f0').
test(x_reduction_eta) :-
  evaluate:x_reduction(e_reduce, 'eta f0', 'f0').

test(evaluate_beta_reduction) :-
  N = lambda(x, application(x-0, x-0)),
  Ni = application(N, N),
  list_to_assoc([f0-N], Bs),
  evaluate:evaluate_reduction(
    'beta f0 f0', Out, Bs, Bsi, [f1], [], 42, 42),
  get_assoc(f1, Bsi, Nii), eq(Ni, Nii),
  W = '(x. x x) (x. x x)',
  atom_to_term(W, TW, normal, 42, 42),
  term_to_atom(TW, Wi, de_bruijn),
  atom_list_concat(['f0 f0 =α= ', W, '\n -β> ', W, '\nf1 = ',
    W, '\n(de Bruijn) ', Wi], Out).

test(evaluate_eta_reduction) :-
  T = lambda(x, application(y-43, x-0)),
  list_to_assoc([f0-T], Bs),
  evaluate:evaluate_reduction(
    'eta f0', Out, Bs, Bsi, [f1], [], 42, 41),
  get_assoc(f1, Bsi, Ti), eq(Ti, y-42),
  atom_list_concat(['f0 =α= x. y x\n -η> y\nf1 = ',
    'y', '\n(de Bruijn) 42'], Out).

:- end_tests(evaluate).

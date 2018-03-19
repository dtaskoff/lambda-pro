:- begin_tests(evaluate).
:- use_module(evaluate).
:- use_module(test_terms).
:- use_module(terms).
:- use_module(helpers).
:- use_module(utils).

test(evaluate_bad_input,
  forall(member(X, [one, two, three]))) :-
    atom_concat('Bad input: ', X, O),
    evaluate:evaluate_bad_input(X, O).

test(evaluate_,
  forall((term(X, name, atom, A),
          term(X, name, lambda, T),
          term(X, de_bruijn, atom, Ai),
          term(X, de_bruijn, lambda, N)))) :-
    empty_assoc(Bs),
    (X == 42 ->
      evaluate:evaluate_(A, N, Out, Bs, Bsi, [f0], [], 42, 41);
      evaluate:evaluate_(A, N, Out, Bs, Bsi, [f0], [], I, I)),
    evaluate:show_terms(f0, A, Ai, Out),
    put_assoc(f0, Bs, (T, N), Bsi).

test(evaluate_lambda,
  forall((term(X, name, atom, A),
          term(X, name, lambda, T),
          term(X, de_bruijn, atom, Ai),
          term(X, de_bruijn, lambda, N)))) :-
    list_to_assoc([f0-(T, N)], Bs),
    evaluate:evaluate_lambda(f0, Out, Bs, Bs, Ns, Ns, I, I),
    evaluate:show_terms(f0, A, Ai, Out).

test(evaluate_substitution,
  forall((term(X, name, lambda, T),
          term(X, de_bruijn, lambda, N)))) :-
    list_to_assoc([f0-(T, N)], Bs),
    evaluate:evaluate_substitution('f0 f0 (x. y)', M, _, Bs, 42, I),
    Ii is I + 2,
    Mi = application(N, application(N, lambda(Ii))),
    eq(M, Mi).

test(x_reduction_beta) :-
  evaluate:x_reduction(b_reduce, 'beta f0 f0', 'f0 f0').
test(x_reduction_eta) :-
  evaluate:x_reduction(e_reduce, 'eta f0', 'f0').

test(evaluate_beta_reduction) :-
  N = lambda(application(0, 0)), Ni = application(N, N),
  list_to_assoc([f0-(_, N)], Bs),
  evaluate:evaluate_reduction(
    'beta f0 f0', Out, Bs, Bsi, [f1], [], 42, 41, b_reduce),
  get_assoc(f1, Bsi, (_, Nii)), eq(Ni, Nii),
  W = '(x. x x) (x. x x)', atom_de_bruijn_atom(W, Wi, 42, 42),
  atom_list_concat(['f0 f0 =α= ', W, ' -β> (', W, ')\nf1 = (',
    W, ')\n(de Bruijn) (', Wi, ')'], Out).

test(evaluate_eta_reduction) :-
  T = lambda(x, application(y, x)),
  N = lambda(application(1, 0)),
  list_to_assoc([f0-(T, N)], Bs),
  evaluate:evaluate_reduction(
    'eta f0', Out, Bs, Bsi, [f1], [], 42, 41, e_reduce),
  get_assoc(f1, Bsi, (_, Ni)), eq(Ni, 1),
  atom_list_concat(['f0 =α= (x. y x) -η> (x)\nf1 = ',
    '(x)', '\n(de Bruijn) (1)'], Out).

:- end_tests(evaluate).

λ-προ is an interpreter for lambda calculus
=====

How to use:
-----
* install a version of [SWI-Prolog](http://www.swi-prolog.org/)
* run SWI-Prolog


```pl
> swipl
Welcome to SWI-Prolog
?- [load]. % load some predicates into swipl
true.

?-
```
* you can play with the available features here
* note: `halt.` exits `swipl`.

```bash ./test.sh``` runs the test suite.

Syntax:
-----
λ-terms are in the following format:
```
x    is a variable
M N  is a λ-application
x. M is a λ-abstraction
```

Currently implemented features:
-----
* convert between user-friendly strings, λ-terms and
  de Bruijn terms which use [de Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index)
* η-reduction for λ-terms using de Bruijn indices

```pl
?- atom_term(x, T).
T = x.

?- atom_term(A, x).
A = x.

?- atom_term('(x y) z', T).
T = application(parentheses(application(x, y)), z).

?- atom_term(A, application(parentheses(application(x, y)), z).
A = '(x y) z'.

?- atom_term('x. x x', T).
T = lambda(x, application(x, x)).

?- atom_term(A, lambda(x, application(x, x))).
A = 'x. x x'.

?- atom_term('x. x y', T), term_de_bruijn(T, N), atom_term(A, N).
... A = 'λ 0 43'.

?- atom_term('x. y. x', T), term_de_bruijn(T, N), atom_term(A, N).
... A = 'λ λ 1'.

?- atom_term('x. y. y x', T), term_de_bruijn(T, N),
|    e_reduce(N, Ni), term_de_bruijn(Ti, Ni), atom_term(A, Ti).
... A = 'x. x'.
```

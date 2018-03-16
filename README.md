λ-προ is an interpreter for lambda calculus
=====

How to use:
-----
* install a version of [SWI-Prolog](http://www.swi-prolog.org/)
* run SWI-Prolog

```bash
> swipl
Welcome to SWI-Prolog
?- [main].
true.

?-
```
* you can play with the available features here
* note: `halt.` exits `swipl`.

Syntax:
-----
λ-terms are in the following format:
```
x     is a variable
(M N) is a λ-application
x. M  is a λ-abstraction
```

Currently implemented features:
-----
* parse a λ-term (and show it):

```pl
?- parse("x", T), show_term(T, S).
T= S, S = x.

?- parse("(x y) z", T), show_term(T, S).
T = application(parentheses(application(x, y)), z),
S = '(x y) z'.

?- parse("x. x x", T), show_term(T, S).
T = lambda(x, application(x, x)),
S = 'x. x x'.

?- parse("x. y. z. (x z) (y z)", T), show_term(T, S).
T = lambda(x, lambda(y, lambda(z, application(application(x, z), application(y, z))))),
S = 'x. y. z. (x z) (y z)'.
```
* convert a λ-term to a de Bruijn term which uses [de Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index) (and show it):

```pl
?- parse("x", T), to_de_bruijn(T, N), show_de_bruijn_term(N, S).
T= x,
N = S, S = 42.

?- parse("(x y) z", T), to_de_bruijn(T, N), show_de_bruijn_term(N, S).
T = application(application(x, y), z),
N = applicaiton(applicaiton(42, 41), 40),
S = '(42 41) 40'.

?- parse("x. x x", T), to_de_bruijn(T, N), show_de_bruijn_term(N, S).
T = lambda(x, application(x, x)),
N = lambda(application(0, 0)),
S = 'λ 0 0'.

?- parse("x. y. z. (x z) (y z)", T), to_de_bruijn(T, N), show_de_bruijn_term(N, S).
T = lambda(x, lambda(y, lambda(z, application(application(x, z), application(y, z))))),
N = lambda(lambda(lambda(application(application(2, 0), application(1, 0))))),
S = 'λ λ λ (2 0) (1 0)'.
```

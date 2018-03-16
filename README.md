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
* parse a λ-term

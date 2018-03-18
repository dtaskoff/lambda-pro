λ-προ is an interpreter for lambda calculus
=====

How to use:
-----
* install a version of [SWI-Prolog](http://www.swi-prolog.org/)


```bash
./repl.sh # start the λ-προ repl
λ-προ: 
```

If you want to play with some of the predicates in `swipl`:
```pl
> swipl
Welcome to SWI-Prolog
?- [load]. % load some predicates into swipl
true.

?-
```

* you can play with the available features here
* note: `halt.` exits `swipl`.

```bash
./test.sh # run the test suite
```

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
* β-reduction and η-reduction for λ-terms using de Bruijn indices
* read-evaluate loop (supports only λ-term reading and
  direct β- and η- reductions)

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

?- atom_de_bruijn_atom('x. x y', A).
A = 'λ 0 43'.

?- atom_de_bruijn_atom('x. y. x', A).
A = 'λ λ 1'.

?- atom_de_bruijn('x. y. y x', N), e_reduce(N, Ni),
|    atom_de_bruijn(A, Ni).
... A = 'x. x'.

?- atom_de_bruijn('(x. x x) (x. x x)', N), b_reduce(N, Ni),
|    atom_de_bruijn(A, Ni).
... A = '(x. x x) (x. x x)'
```

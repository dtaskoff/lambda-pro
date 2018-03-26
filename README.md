λ-προ is an interpreter for lambda calculus
=====


Currently implemented features:
-----
* convert between user-friendly strings, λ-terms and
  de Bruijn terms which use [de Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index)
* read-evaluate loop:
  - λ-term reading and printing
  - β- and η- reductions
  - α-equivalence checking (for already defined terms)

How to use:
-----
Install a version of [SWI-Prolog](http://www.swi-prolog.org/).


```bash
$ make test    # run the test suite
$ make         # build the λ-προ executable
$ ./lambda-pro # start the λ-προ repl
λ-προ: quit    % exit the λ-προ repl
$
```

#### What is a λ-term?

```
<λ-term> ::= <λ-abstraction> | <application> | <variable> | (<λ-term>)
<λ-abstraction> ::= <variable>. <λ-term>
<application> ::= <λ-term> <λ-term>
<variable> ::= s where s ∈ Σ⁺
```

Knowing that, you can define λ-terms in the repl:
```
λ-προ: x. x x
f0 = x. x x
λ-προ: x. y. x
f1 = x. y. x
λ-προ: x. y. y
f2 = x. y. y
```
Showing the names in the current environment is done like that:
```
λ-προ: env
f0 = x. x x
f1 = x. y. x
f2 = x. y. y
```
It's possible to show de Bruijn indices of a λ-term:
```
λ-προ: f0?
f0 = x. x x
λ 0 0
```
And reference other terms by name:
```
λ-προ: f0 f0
f3 = f0 f0
```
You can see a single step of β-reduction:
```
λ-προ: beta f3
 -β> 
f4 = (x. x x) x. x x
```
Or a single step of η-reduction:
```
λ-προ: x. y. x y
f5 = x. y. x y
λ-προ: eta f5
 -η> 
f6 = x. x
```
Or check for α-equivalence of λ-terms:
```
λ-προ: x. x y
f7 = x. x y
λ-προ: u. u v
f8 = u. u v
λ-προ: u. v u
f9 = u. v u
λ-προ: f7 == f8
x. x y =α= u. u v ?
true
λ-προ: f8 == f9
u. u v =α= u. v u ?
false
```
There are also transitive and reflexive closures for the reductions:
```
λ-προ: c3 = f. x. f (f (f x)) 
c3 = f. x. f (f (f x))
λ-προ: beta* c3 succ zero
 -β>> 
f10 = succ (succ (succ zero))
λ-προ: x. y. z. x y z
f11 = x. y. z. x y z
λ-προ: eta* f11
 -η>> 
f12 = x. x
```
You can also load a file with defined λ-terms (see [this one](./church.lpro) for example):
```
λ-προ: load church.lpro
λ-προ: K*?
K* = x. y. y
λ λ 0
```

#### swipl

If you want to play with some of the predicates in `swipl`:
```bash
$ swipl
Welcome to SWI-Prolog
?- [load]. % load some predicates into swipl
true.

?- halt.
$
```

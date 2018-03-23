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
f3
 =ρ= f0 f0
 =ρ= (x. x x) (x. x x)
 -β> (x. x x) (x. x x)
f4 = (x. x x) (x. x x)
```
Or a single step of η-reduction:
```
λ-προ: x. y. x y
f5 = x. y. x y
λ-προ: eta f5
f5
 =ρ= x. y. x y
 -η> x. x
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
λ-προ: c5 = f. x. f (f (f x)) 
c5 = f. x. f (f (f x))
λ-προ: beta* c3 succ zero
c3 succ zero
 =ρ= (f. x. f (f (f x))) succ zero
 -β>> succ (succ (succ zero))
f10 = succ (succ (succ zero))
λ-προ: x. y. z. x y z
f11 = x. y. z. x y z
λ-προ: eta* f11
f11
 =ρ= x. y. z. x y z
 -η>> x. x
f12 = x. x
```
You can also load a file with defined λ-terms (see [this one](./combinators.lpro) for example):
```
λ-προ: load combinators.lpro

I = x. x
K = x. y. x
K* = x. y. y
S = x. y. z. (x z) (y z)
w = x. x x
W = w w
Y = f. (x. f (x x)) (x. f (x x))
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

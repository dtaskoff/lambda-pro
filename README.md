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
$ ./test.sh # run the test suite
$ ./repl.sh # start the λ-προ repl
λ-προ: quit # exit the λ-προ repl
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
(de Bruijn) λ 0 0
λ-προ: x. y. x
f1 = x. y. x
(de Bruijn) λ λ 1
λ-προ: x. y. y
f2 = x. y. y
(de Bruijn) λ λ 0
```
It's possible to reference other terms by name:
```
λ-προ: f0?
f0 = x. x x
(de Bruijn) λ 0 0
λ-προ: f0 f0
f3 = f0 f0
(de Bruijn) 4200 4200
```
You can see a single step of β-reduction:
```
λ-προ: beta f3
f3
 =ρ= f0 f0
 =ρ= (x. x x) (x. x x)
 -β> (x. x x) (x. x x)
f4 = (x. x x) (x. x x)
(de Bruijn) (λ 0 0) (λ 0 0)
```
Or a single step of η-reduction:
```
λ-προ: x. y. x y
f5 = x. y. x y
(de Bruijn) λ λ 1 0
λ-προ: eta f5
f5
 =ρ= x. y. x y
 -η> x. x
f6 = x. x
(de Bruijn) λ 0
```
Or check for α-equivalence of λ-terms:
```
λ-προ: x. x y
f7 = x. x y
(de Bruijn) λ 0 4200
λ-προ: u. u v
f8 = u. u v
(de Bruijn) λ 0 4199
λ-προ: u. v u
f9 = u. v u
(de Bruijn) λ 4198 0
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
(de Bruijn) λ λ 1 (1 (1 0))
λ-προ: beta* c5 succ zero
c5 succ zero
 =ρ= (f. x. f (f (f x))) succ zero
 -β>> succ (succ (succ zero))
f10 = succ (succ (succ zero))
(de Bruijn) 4195 (4195 (4195 4194))
λ-προ: x. y. z. x y z
f11 = x. y. z. x y z
(de Bruijn) λ λ λ 2 1 0
λ-προ: eta* f11
f11
 =ρ= x. y. z. x y z
 -η>> x. x
f12 = x. x
(de Bruijn) λ 0
```
You can also load a file with defined λ-terms (see [this one](./combinators.lpro) for example):
```
λ-προ: load combinators.lpro

I = x. x
(de Bruijn) λ 0
K = x. y. x
(de Bruijn) λ λ 1
K* = x. y. y
(de Bruijn) λ λ 0
S = x. y. z. (x z) (y z)
(de Bruijn) λ λ λ (2 0) (1 0)
w = x. x x
(de Bruijn) λ 0 0
W = w w
(de Bruijn) 4196 4196
Y = f. (x. f (x x)) (x. f (x x))
(de Bruijn) λ (λ 1 (0 0)) (λ 1 (0 0))
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

# Lambda Calculus
## Conversions:
* **substitution** - renaming of unbound variables, e.g. `(λa.ab)[d/b] == (λa.ad)` or `(λx.xy(λy.yx))[z/y] == (λx.xz(λy.yx))`
* **alpha conversion** - renaming of bound variables, e.g. `(λy.e)[x/y] = (λx.e)` or `(λt.(x(λx.xt)))[y/x] = (λt.(x(λy.yt)))` (notice that the bound `x` is not substituted), so `(λx.xy(λy.yx))[z/y] == (λx.xy(λz.zx))`
* **beta conversion** - the only computation possible, e.g. `(λx.e)f == e[f/x]` or `(λx.x)(λz.z) = x[(λz.z)/x]`
* **eta conversion** - simplification of terms without change of their effect, e.g. `(λy.λx.yx) == (λyx.yx) == (λy.y)`

When performing substitution and alpha conversion, check: 
* that all bound variables are still bound and have the same binding,
* that all unbound variables are still unbound.

## Bracket Conventions
* `((((E1E2)E3)E4)E5) == E1E2E3E4E5`
* `(λV.(E1E2E3)) == λV.E1E2E3`
* `(λV1.(λV2.(λV3.E))) == λV1V2V3.E`

### Attention (Wrong)
* `((((E1E2)E3)E4)E5) != (E1(E2(E3(E4E5))))`
* `(λV.(E1E2E3))E4 == (λV.E1E2E3)E4`
* `(λV1.(λV2.(λV3.E1)))E2 == (λV1V2V3.E1)E2`

## Proves
proving with structural induction <var>
1. <var> = <value>
    * **proving: L expression = R expression**
    * simplification of L
    * simplification of R
    * L = R
2. <var> = <value>
    * IP: something based on the result of 1
    * **proving: L expression = R expression**
    * simplification of L
    * simplification of R
    * L = R
    * Q.E.D

## Fixed Point
* `YE = E(YE)`

### MINUS
```
LET T = λxy.x    LET Z = isZero
LET F = λxy.y    LET P = prev   // prev 0 = 0
LET 0 = λzn.n    LET ADD = +

def MINUS a b:
    if Z b:
        return a
    else:
        return MINUS (P a) (P b)

LET MINUS = Y(λfab.(Z b)(a)(f (P a) (P b)))

def MUL a b:
    if Z b:
        return 0
    else:
        return add a (MUL a (P b))

LET MUL = Y(λfab.(Z b)(0)(ADD a (f a (P b))))
``` 

## Identity, Equality, Relation
* Identity: e.g. `λx.x` is identical only to `λx.x`
* Equality (Relation -> or Relation <-): e.g. `λx.x` is equal to `λy.y` (alpha reduction), but also to `(λxy.y)z` (beta reduction followed by alpha reduction)
* Relation ->: e.g. `(λabc.abc)uv -> uv` (2 beta reductions followed by eta reduction)

# Haskell
* **foldl** computes list from start to end (**cannot** be used on infinite lists) 
    - `(b -> a -> b) -> b -> [a] -> b` (function takes accumulator as first argument), official data signature `(a -> b -> a) -> a -> [b] -> a`
* **foldr** computes list from end to start (**can** be used on infinite lists)
    - `(a -> b -> b) -> b -> [a] -> b` (function takes accumulator as second argument)
* **curry** transforms a function taking tuple as a parameter to a function taking 2 arguments
    - `((a, b) -> c) -> a -> b -> c`
    - (curry sumPair) 10 5 == sumParams 10 5
* **uncurry** transforms a function taking two parameters into a function taking tuple
    - `(a -> b -> c) -> (a, b) -> c`
    - (uncurry sumParams) (8, 3) == sumPair (8, 3)

## Function Composition
* **f . g** applies the function `g` and then the function `f`, e.g. addSumCheck add check = `(>check) . sum . map (+add)`
    - `(b -> c) -> (a -> b) -> a -> c`

## Class Instances
* Show:    `instance (Show k, Show v) => Show (Tree k v) where`
* Ord:     `instance (Eq a) => Ord (MyType a) where`, or if the MyType is Ord already then `instance Ord MyType where`
* Functor: `instance (Ord k) => Functor (Tree k) where` (only `Tree k`, even if the data type is `Tree k v`)

## Functors
* `unpackFunctor = foldr ((<*>) . ((<$>) (:))) (pure [])`
* `addJust val = map ((<$>) (+val))`

## Useful Functions
* `takeWhile` - copies a list until a given condition is satisfied
* `dropWhile` - removes leading part of a list until a given condition is satisfied
* `span`      - separates a list where a given condition is satisfied, e.g. `span (/= ' ') "Hi :D" = ("Hi", " :D")`

## Parsing Data Types
* `read "123" :: Int`
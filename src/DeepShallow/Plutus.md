# Philip Wadler, Smart Contracts

### _Unbounded integers_, _Abstract Data Types_ and _Data Constructors_.

https://youtu.be/IqA-mI2olFA

Miranda has a better way of defining Abstract data types.

```hs
-- Abstract data types in Miranda
abstype stack
with empty   :: stack
     isempty :: stack -> bool
     push    :: num -> stack -> stack
     pop     :: stack -> stack
     top     :: stack -> num

-- What does equality means here?
-- Is this means that stack constructor is the same
-- as list constructor (and can pattern match as list)?
stack == [num]

empty     = []
isempty x = null x
push a x  = a:x
pop (a:x) = x
top (a:x) = a


-- Abstract data types in Haskell
module Stack (Stack, empty, isempty, push, pop, top) where

-- modified to use type alias instead of newtype
-- so it cannot work with typeclass (actually can with FlexibleInstances)
-- and constructor is not hidden
-- BUT how this is different from Miranda version?
type Stack = [Int]

empty     = []
isempty x = null x
push a x  = a:x
pop (a:x) = x
top (a:x) = a
```
Data constructors

```hs
validator :: A -> comp B
redeemer  :: comp A

-- validator creates ADT for use by redeemer
validator :: (∀ x. A[x] -> comp B[x]) -> comp C

-- should this be `comp (∀x. A[x] -> comp B[x])`?
redeemer  :: ∀ x. A[x] -> comp B[x]

-- to get `comp B[s]` validator gives stack implementation to redeemer
validator :: (∀ s. s (s->bool)->(num->s->s)->(s->s)->(s->num) -> comp B[s])
          -> comp C
validator redeemer =
  let answer = redeemer stack empty isEmpty push pop top
  in ... do suff with answer ...
```
Inductive data type constructors

```hs
data Nat = Zero | Suc Nat

-- "Constructors used in pattern matching are not just functions."
plus Zero    n = n
plus (Suc m) n = Suc (plus m n)

-- Rho-haskell style :)
-- functions with holes? in Rho patterns are processes with holes
plus @Zero    n = *n
plus @(Suc m) n = Suc (plus m *n)
```

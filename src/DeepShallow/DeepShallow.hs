{-# LANGUAGE FlexibleInstances #-}

module DeepShallow.DeepShallow where

import Control.Applicative

-- Concrete structure, deep embedding (initial encoding).
-- Represents something like a dictionary or key-value store.
myStruct, myStruct' :: [(String, Integer)]
myStruct  = [("a", 1), ("b", 2), ("c", 3)]

-- List without syntactic sugar.
-- (,) and (:) functions creates concrete structure (Product and List)
myStruct' = ("a", 1) : ("b", 2) : ("c", 3) : []

{-
  First abstraction, product constructor is apstracted and supplied
  to the function, List is still concrete.
-}

kv1 :: (String -> Integer -> a) -> [a]
kv1 f = [f "a" 1, f "b" 2, f "c" 3]

-- To get concrete structure apply "abstract definition" to product constructor.
myStruct1 :: [(String, Integer)]
myStruct1 = kv1 (,)

-- List Cons constructor (:) can be replaced with compose function (.) and empty
-- list will be supplied later when constructing concrete value.
-- (.) :: (b -> c) -> (a -> b) -> a -> c
kv1a :: (String -> Integer -> a -> a) -> a -> a
kv1a f = f "a" 1
       . f "b" 2
       . f "c" 3

-- Composition can also be a Semigroup `mappend` (<>) (Monoid is Semigroup with Unit).
-- (<>) implementation for List is concat (++)
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-290
kv1b :: (Semigroup a) => (String -> Integer -> a) -> a
kv1b f = f "a" 1
      <> f "b" 2
      <> f "c" 3

-- Composition with Alternative - an associative binary operation.
-- (<|>) implementation for List is concat (++)
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-993
kv1c :: (Alternative m) => (String -> Integer -> m a) -> m a
kv1c f = f "a" 1
     <|> f "b" 2
     <|> f "c" 3

-- Helper "transform function" which applies `f` to structure constructors
-- and produce concrete structure.
-- It works with both, function composition (.) and semigroup mappend (<>).
concreteComp :: ((a -> b -> [(a, b)] -> [(a, b)]) -> [c] -> t) -> t
concreteComp f = f collect [] where
  collect :: a -> b -> [(a, b)] -> [(a, b)]
  collect = curry (:)

{-

Works with (<>) because of Semigroup instance for functions.
`instance Semigroup b => Semigroup (a -> b)` - return type `b` must also be a Semigroup
http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-328

Reduction for composition with function composition (.)

  ([(a, b)]->[(a, b)]) . ([(a, b)]->[(a, b)])
= (x->x) . (x->x)                                -- x = [(a, b)]
= \x = (x->x) ((x->x) x)                         -- f . g = \x -> f (g x)
= \x = (x->x) x
= \x = x
= id

Reduction for composition with semigroup mappend (<>)

  ([(a, b)]->[(a, b)]) <> ([(a, b)]->[(a, b)])
= (x->x) <> (x->x)                               -- x = [(a, b)]
= \x -> (x->x) x <> (x->x) x                     -- f <> g = \x -> f x <> g x
= \x -> x <> x
= \x -> x ++ x                                   -- (<>) = (++)
= \x -> x
= id

-}

-- This works for `f` with semigroup composition.
-- `curry` is product constructor and (:[]) is singleton List constructor.
concreteList :: ((a -> b -> [(a, b)]) -> t) -> t
concreteList f = f collect where
  collect :: a -> b -> [(a, b)]
  collect = curry (:[])

-- We can choose Maybe instead of List. What is changed is only the constructor
-- Just instead of (:[]).
concreteMaybe :: ((a -> b -> Maybe (a, b)) -> t) -> t
concreteMaybe f = f collect where
  collect :: a -> b -> Maybe (a, b)
  collect = curry Just

-- We can abstract over concrete data type because what we only need is constructor
-- for this type. This is exactly Applicative `pure :: a -> f a` function.
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#Applicative
concrete :: Applicative m => ((a -> b -> m (a, b)) -> t) -> t
concrete f = f $ curry pure

-- [], Maybe are concrete data types, and they have implementation for Applicative.
-- Now functions for List and Maybe can be defined just by choosing concrete type
-- that implements Applicative. This means that we don't have to define these
-- functions at all. Type inference will choose the right implementation.
concreteList' :: ((a -> b -> [(a, b)]) -> t) -> t
concreteList' = concrete

concreteMaybe' :: ((a -> b -> Maybe (a, b)) -> t) -> t
concreteMaybe' = concrete

concreteIO' :: ((a -> b -> IO (a, b)) -> t) -> t
concreteIO' = concrete

-- All concrete structures are equal created from "shallow definitions".
isEqual = all (==myStruct) [
    concreteComp  kv1a,
    concreteComp  kv1b,
    concreteList  kv1b,
    concreteList' kv1b,
    concrete      kv1c
  ]

-- Maybe as a structure gives first element.
first1c :: Maybe (String, Integer)
first1c = concrete kv1c

first1c' :: Maybe (String, Integer)
first1c' = concreteMaybe kv1c

{-
  Second abstraction, composition function is also abstracted and it is
  supplied to the function. Now the definition doesn't have any constraint
  on composition implementation.
-}

-- Abstract structure, shallow embedding (final encoding).
-- Represents something like a dictionary or key-value store.
kv2 :: (a -> a -> a) -> (String -> Integer -> a) -> a
kv2 (&|) f = f "a" 1
          &| f "b" 2
          &| f "b" 22
          &| f "c" 3

-- Of course we can easily get all previous definitions,
-- partially apply with correct "composition" function.
kv1a' :: (String -> Integer -> a -> a) -> a -> a
kv1a' = kv2 (.)

kv1b' :: (Semigroup a) => (String -> Integer -> a) -> a
kv1b' = kv2 (<>)

kv1c' :: (Alternative m) => (String -> Integer -> m a) -> m a
kv1c' = kv2 (<|>)

{-
  We can now write functions that are more general and which create only
  part of the structure that is really needed.
-}

-- Filter by "key". This function is very similar to `concrete` function
-- with addition of predicate to pick some of values. Applicative
-- constraint is not enough because when predicate is False we need notion of
-- `empty` value which is provided with Alternative typeclass.
filterBy :: Alternative m => (a -> Bool) -> ((a -> b -> m (a, b)) -> t) -> t
filterBy p f = f findPair where
  findPair a b = if p a then pure (a, b) else empty

-- Filter "b"s, but without choosing concrete structure. How values are collected
-- depends on the instance of `m`.
filterBs :: Alternative m => m (String, Integer)
filterBs = filterBy (=="b") $ kv2 (<|>)

-- For Maybe instance this means, get the first value.
-- instance Alternative Maybe
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-897
bVal :: Maybe (String, Integer)
bVal = filterBs -- Just ("b",2)

-- For List instance this means concatenate all values.
-- instance Alternative []
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-993
bVals :: [(String, Integer)]
bVals = filterBs -- [("b",2),("b",22)]

-- To sum all values we don't need to create any structure. This is like fold
-- over functions.
sumKv :: Num a => ((a -> a -> a) -> (b -> c -> c) -> t) -> t
sumKv f = f (+) (flip const)

sumVals :: Integer
sumVals = sumKv kv2 -- 28

{-
  TypeClasses
-}

-- TypeClasses are like dictionary of functions so instead depending of functions
-- directly we can define them in MyKeyValueStore TypeClass.
class MyKeyValueStore a where
  (.>) :: String -> Integer -> a
  (&|) :: a -> a -> a

-- We want to precendence of composition be less then combining operator: key .> value.
infixl 8 &|

-- Now definition depends on the TypeClass, not functions separately.
myExp :: MyKeyValueStore a => a
myExp = "a" .> 1
     &| "b" .> 2
     &| "c" .> 3

-- Implementation for MyKeyValueStore for any Alternative structure.
instance Alternative m => MyKeyValueStore (m (String, Integer)) where
  (&|) = (<|>)
  (.>) = curry pure

-- Implementation how to sum all values.
instance MyKeyValueStore Integer where
  (&|) = (+)
  (.>) = flip const

-- These are the same as bVal, bVals and sumVals. Choosing the type determines
-- which implementation (tree) will be selected.

myExpFirst :: Maybe (String, Integer)
myExpFirst = myExp

myExpList :: [(String, Integer)]
myExpList = myExp

myExpSum :: Integer
myExpSum = myExp

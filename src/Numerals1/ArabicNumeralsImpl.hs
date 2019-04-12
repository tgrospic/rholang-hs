module Numerals1.ArabicNumeralsImpl
  ( eval1, isStructEq1, isEq1
  , AN (..), eval2, isStructEq2, isEq2
  , AN3 (..), eval3, isStructEq3, isEq3, show3
  , AN4 (..), eval4, isStructEq4, isEq4, show4
  , testNumerals1Eq
  , toNum
  ) where

import Numerals1.ArabicNumerals
import Control.Monad
import Data.Digits
import Data.List (intercalate)
import Data.MultiSet

toNum xs = unDigits 10 $ digitsZeroFix =<< xs
  where
  digitsZeroFix a = case digits 10 a of
    [] -> [0]
    a  -> a

-- Shallow embedding directly to Int

instance ArabicNumerals1Symantics Int where
  zero    = 0
  one     = 1
  two     = 2
  three   = 3
  four    = 4
  five    = 5
  six     = 6
  seven   = 7
  eight   = 8
  nine    = 9
  a # b   = toNum [a, b]
  a .+ b  = a + b

eval1 :: Int -> Int
eval1 = id

isStructEq1 :: Int -> Int -> Bool
isStructEq1 a b = a == b

isEq1 a b = eval1 a == eval1 b

-- isStructEq1 ≡ isEq1


-- With more structure we can have structural equality
-- for (#) but not for (.+)

newtype AN = AN [Int] deriving (Eq, Show)

instance ArabicNumerals1Symantics AN where
  zero          = AN [0]
  one           = AN [1]
  two           = AN [2]
  three         = AN [3]
  four          = AN [4]
  five          = AN [5]
  six           = AN [6]
  seven         = AN [7]
  eight         = AN [8]
  nine          = AN [9]
  -- syntax erasure    zero # x == x
  AN [0] # AN b = AN b
  AN a   # AN b = AN $ a ++ b
  a     .+ b    = AN [eval2 a + eval2 b]

eval2 (AN a) = toNum a

isStructEq2 :: AN -> AN -> Bool
isStructEq2 a b = a == b

isEq2 a b = eval2 a == eval2 b


-- Next step is to add structure for addition (.+), deep embedding

newtype AN3 = AN3 [Op3] deriving (Eq, Show)

data Op3 = Plus [Op3] [Op3] | Dig Int deriving (Eq, Ord, Show)

instance ArabicNumerals1Symantics AN3 where
  zero                = AN3 [Dig 0]
  one                 = AN3 [Dig 1]
  two                 = AN3 [Dig 2]
  three               = AN3 [Dig 3]
  four                = AN3 [Dig 4]
  five                = AN3 [Dig 5]
  six                 = AN3 [Dig 6]
  seven               = AN3 [Dig 7]
  eight               = AN3 [Dig 8]
  nine                = AN3 [Dig 9]
  -- syntax erasure    zero # x == x
  AN3 [Dig 0] # AN3 b = AN3 b
  AN3 a       # AN3 b = AN3 (a ++ b)
  -- syntax erasure    zero + x == x
  AN3 [Dig 0] .+ AN3 b    = AN3 b
  -- syntax erasure    x + zero == x
  AN3 a .+ AN3 [Dig 0]    = AN3 a
  -- ordering gives symmetry to addition:   1 + 2 ≡ 2 + 1
  -- but associativity does not hold: (1 + 2) + 3 ≢ 1 + (2 + 3)
  AN3 a .+ AN3 b | a <= b = AN3 [Plus a b]
  AN3 a .+ AN3 b | a > b  = AN3 [Plus b a]

eval3 (AN3 xs) = toNum $ evalOps xs
  where
  evalOps = foldr eval []
  eval (Dig x)    acc = x : acc
  eval (Plus x y) acc = toNum (evalOps x) + toNum (evalOps y) : acc

isStructEq3 :: AN3 -> AN3 -> Bool
isStructEq3 a b = a == b

isEq3 a b = eval3 a == eval3 b

---------------- SHOW AN3
show3 (AN3 xs) = showOps xs
  where
  showOps = foldr showOp ""
  showOp (Dig x)    acc = show x ++ acc
  showOp (Plus x y) acc = "(" ++ showOps x ++ "+" ++ showOps y ++ ")" ++ acc


-- Final solution is to use MultiSet for addition (.+) commutativity

newtype AN4 = AN4 [Op4] deriving (Eq, Show)

data Op4 = Addition (MultiSet [Op4]) | Digit Int deriving (Eq, Ord, Show)

instance ArabicNumerals1Symantics AN4 where
  zero                  = AN4 [Digit 0]
  one                   = AN4 [Digit 1]
  two                   = AN4 [Digit 2]
  three                 = AN4 [Digit 3]
  four                  = AN4 [Digit 4]
  five                  = AN4 [Digit 5]
  six                   = AN4 [Digit 6]
  seven                 = AN4 [Digit 7]
  eight                 = AN4 [Digit 8]
  nine                  = AN4 [Digit 9]
  -- syntax erasure    zero # x == x
  AN4 [Digit 0] # AN4 b = AN4 b
  AN4 a         # AN4 b = AN4 (a ++ b)
  -- syntax erasure    zero + x == x
  AN4 [Digit 0] .+ AN4 b = AN4 b
  -- syntax erasure    x + zero == x
  AN4 a .+ AN4 [Digit 0] = AN4 a
  -- MultiSet structure gives commutativity: 1 + 2 ≡ 2 + 1
  -- and forgetting history gives associativity: (1 + 2) + 3 ≡ 1 + (2 + 3)
  AN4 [Addition a] .+ AN4 [Addition b] = AN4 [Addition (a `union` b)]
  AN4 [Addition a] .+ AN4 b            = AN4 [Addition (b `insert` a)]
  AN4 a            .+ AN4 [Addition b] = AN4 [Addition (a `insert` b)]
  AN4 a            .+ AN4 b            = AN4 [Addition (fromList [a, b])]

eval4 (AN4 xs) = toNum $ evalOps xs
  where
  evalOps = foldr eval []
  evalSum xs = sum $ toNum . evalOps <$> toList xs
  eval (Digit x)    acc = x : acc
  eval (Addition x) acc = evalSum x : acc

isStructEq4 :: AN4 -> AN4 -> Bool
isStructEq4 a b = a == b

isEq4 a b = eval4 a == eval4 b

---------------- SHOW AN4
show4 (AN4 xs) = showOps xs
  where
  showOps = foldr showOp ""
  showSum xs = intercalate "+" (showOps <$> toList xs)
  showOp (Digit x)     acc = show x ++ acc
  showOp (Addition xs) acc = "(" ++ showSum xs ++ ")" ++ acc

{-

Structural equivalence and operational semantics with different implementation structures

-}
testNumerals1Eq = [
    "> two .+ three `isEq1` (zero # five)", e1 isEq1,
    "> two .+ three `isEq2` (zero # five)", e1 isEq2,
    "> two .+ three `isEq3` (zero # five)", e1 isEq3,
    "> two .+ three `isEq4` (zero # five)", e1 isEq4,
    "",
    "> two .+ three `isStructEq1` five", e1 isStructEq1,
    "> two .+ three `isStructEq2` five", e1 isStructEq2,
    "> two .+ three `isStructEq3` five", e1 isStructEq3,
    "> two .+ three `isStructEq4` five", e1 isStructEq4,
    "",
    "> one # two `isStructEq1` (one # (one .+ one))", e3 isStructEq1,
    "> one # two `isStructEq2` (one # (one .+ one))", e3 isStructEq2,
    "> one # two `isStructEq3` (one # (one .+ one))", e3 isStructEq3,
    "> one # two `isStructEq4` (one # (one .+ one))", e3 isStructEq4,
    "",
    "> two .+ three `isStructEq1` (three .+ two)", e4 isStructEq1,
    "> two .+ three `isStructEq2` (three .+ two)", e4 isStructEq2,
    "> two .+ three `isStructEq3` (three .+ two)", e4 isStructEq3,
    "> two .+ three `isStructEq4` (three .+ two)", e4 isStructEq4,
    "",
    "> ((one .+ two) .+ three) `isStructEq1` (one .+ (two .+ three))", e5 isStructEq1,
    "> ((one .+ two) .+ three) `isStructEq2` (one .+ (two .+ three))", e5 isStructEq2,
    "> ((one .+ two) .+ three) `isStructEq3` (one .+ (two .+ three))", e5 isStructEq3,
    "> ((one .+ two) .+ three) `isStructEq4` (one .+ (two .+ three))", e5 isStructEq4
  ]
  where
    e1 f = show $ two .+ three `f` (zero # five)
    e2 f = show $ two .+ three `f` five
    e3 f = show $ one # two `f` (one # (one .+ one))
    e4 f = show $ two .+ three `f` (three .+ two)
    e5 f = show $ ((one .+ two) .+ three) `f` (one .+ (two .+ three))

{-

In the first and second case addition structure is not preserved

> two .+ three `isEq1` (zero # five)
True
> two .+ three `isEq2` (zero # five)
True
> two .+ three `isEq3` (zero # five)
True
> two .+ three `isEq4` (zero # five)
True

> two .+ three `isStructEq1` five
True
> two .+ three `isStructEq2` five
True
> two .+ three `isStructEq3` five
False
> two .+ three `isStructEq4` five
False

> one # two `isStructEq1` (one # (one .+ one))
True
> one # two `isStructEq2` (one # (one .+ one))
True
> one # two `isStructEq3` (one # (one .+ one))
False
> one # two `isStructEq4` (one # (one .+ one))
False

> two .+ three `isStructEq1` (three .+ two)
True
> two .+ three `isStructEq2` (three .+ two)
True
> two .+ three `isStructEq3` (three .+ two)
True
> two .+ three `isStructEq4` (three .+ two)
True

> ((one .+ two) .+ three) `isStructEq1` (one .+ (two .+ three))
True
> ((one .+ two) .+ three) `isStructEq2` (one .+ (two .+ three))
True
> ((one .+ two) .+ three) `isStructEq3` (one .+ (two .+ three))
False
> ((one .+ two) .+ three) `isStructEq4` (one .+ (two .+ three))
True

-}

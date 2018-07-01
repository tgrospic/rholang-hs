module Numerals.ArabicNumeralsImpl
  ( eval1, isStructEq1, isEq1
  , AN (..), eval2, isStructEq2, isEq2
  , AN3 (..), eval3, isStructEq3, isEq3, eraseZeroOp3, show3, sshow3
  , AN4 (..), eval4, isStructEq4, isEq4, eraseZeroOp4, show4, sshow4
  , testNumeralsEq
  , toNum
  ) where

import Numerals.ArabicNumerals
import Prelude hiding (map)
import Control.Monad
import Data.Digits
import Data.List hiding (map)
import Data.MultiSet

toNum xs = unDigits 10 $ digits 10 =<< xs

-- Shallow embedding directly to Int

instance ArabicNumeralsSymantics Int where
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

instance ArabicNumeralsSymantics AN where
  zero        = AN [0]
  one         = AN [1]
  two         = AN [2]
  three       = AN [3]
  four        = AN [4]
  five        = AN [5]
  six         = AN [6]
  seven       = AN [7]
  eight       = AN [8]
  nine        = AN [9]
  AN a # AN b = AN $ a ++ b
  a   .+ b    = AN [eval2 a + eval2 b]

eval2 (AN a) = toNum a

isStructEq2 :: AN -> AN -> Bool
isStructEq2 a b = trimZero a == trimZero b
  where
  -- Syntactic erasure: 00420 == 420
  trimZero (AN xs) = dropWhile (==0) xs

isEq2 a b = eval2 a == eval2 b


-- Next step is to add structure for addition (.+), deep embedding

newtype AN3 = AN3 [Op3] deriving (Eq, Show)

data Op3 = Plus [Op3] [Op3] | Dig Int deriving (Eq, Show)

instance ArabicNumeralsSymantics AN3 where
  zero           = AN3 [Dig 0]
  one            = AN3 [Dig 1]
  two            = AN3 [Dig 2]
  three          = AN3 [Dig 3]
  four           = AN3 [Dig 4]
  five           = AN3 [Dig 5]
  six            = AN3 [Dig 6]
  seven          = AN3 [Dig 7]
  eight          = AN3 [Dig 8]
  nine           = AN3 [Dig 9]
  AN3 a  # AN3 b = AN3 (a ++ b)
  AN3 a .+ AN3 b = AN3 [Plus a b]

eval3 (AN3 xs) = toNum $ fold xs
  where
  fold = foldr eval []
  eval (Dig x)    acc = x : acc
  eval (Plus x y) acc = toNum (fold x) + toNum (fold y) : acc

isStructEq3 :: AN3 -> AN3 -> Bool
isStructEq3 (AN3 a) (AN3 b) = eraseZeroAddSemOp3 a == eraseZeroAddSemOp3 b

-- First attempt - Plus [Op3] [Op3]
-- 1 + 2 /≡ 2 + 1
eraseZeroOp3 xs = foldr synErase [] (trimZero xs)
  where
  -- Syntactic erasure: 00420 == 420
  synErase (Plus x y) xs = Plus (trimZero x) (trimZero y) : xs
  synErase x          xs = x : xs
  -- Remove leading 0's
  trimZero = dropWhile isZero
  isZero (Dig 0) = True
  isZero _       = False

-- Easiest way to just use Op4 structure (with MultiSet for addition)
-- 1 + 2 ≡ 2 + 1
eraseZeroAddSemOp3 = eraseZeroOp4 . (<$>) convertToOp4
  where
  -- from pair to MultiSet - commutativity for addition
  convertToOp4 (Plus x y) = Addition (fromList [convertToOp4 <$> x, convertToOp4 <$> y])
  convertToOp4 (Dig x)    = Digit x

isEq3 a b = eval3 a == eval3 b


---------------- SHOW AN3
sshow3 (AN3 xs) = eraseZeroOp3 xs

show3 (AN3 xs) = showOp xs
  where
  showOp = foldr eval ""
  eval (Dig x)    acc = show x ++ acc
  eval (Plus x y) acc = "(" ++ showOp x ++ "+" ++ showOp y ++ ")" ++ acc


-- Final solution is to use MultiSet for addition (.+) commutativity

newtype AN4 = AN4 [Op4] deriving (Eq, Show)

data Op4 = Addition (MultiSet [Op4]) | Digit Int deriving (Eq, Ord, Show)

instance ArabicNumeralsSymantics AN4 where
  zero           = AN4 [Digit 0]
  one            = AN4 [Digit 1]
  two            = AN4 [Digit 2]
  three          = AN4 [Digit 3]
  four           = AN4 [Digit 4]
  five           = AN4 [Digit 5]
  six            = AN4 [Digit 6]
  seven          = AN4 [Digit 7]
  eight          = AN4 [Digit 8]
  nine           = AN4 [Digit 9]
  AN4 a  # AN4 b = AN4 (a ++ b)
  AN4 a .+ AN4 b = AN4 [Addition (fromList [a, b])]

eval4 (AN4 xs) = toNum $ evalOp xs
  where
  evalOp = foldr eval []
  evalSum xs = sum $ toNum . evalOp <$> toList xs
  eval (Digit x)    acc = x : acc
  eval (Addition x) acc = evalSum x : acc

-- MultiSet structure gives commutativity for addition
-- 1 + 2 ≡ 2 + 1
isStructEq4 :: AN4 -> AN4 -> Bool
isStructEq4 (AN4 a) (AN4 b) = eraseZeroOp4 a == eraseZeroOp4 b

eraseZeroOp4 xs = foldr synErase [] (trimZero xs)
  where
  -- Syntactic erasure: 00420 == 420
  synErase (Addition x) xs = Addition (map trimZero x) : xs
  synErase x            xs = x : xs
  -- Remove leading 0's
  trimZero = dropWhile isZero
  isZero (Digit 0) = True
  isZero _         = False

isEq4 a b = eval4 a == eval4 b

---------------- SHOW AN4
sshow4 (AN4 xs) = eraseZeroOp4 xs

show4 (AN4 xs) = showOp xs
  where
  showOp = foldr eval ""
  showSum xs = intercalate "+" (showOp <$> toList xs)
  eval (Digit x)     acc = show x ++ acc
  eval (Addition xs) acc = "(" ++ showSum xs ++ ")" ++ acc

{-

Structural equivalence and operational semantics with different implementation structures

-}
testNumeralsEq = [
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
    "> two .+ three `isStructEq4` (three .+ two)", e4 isStructEq4
  ]
  where
    e1 f = show $ two .+ three `f` (zero # five)
    e2 f = show $ two .+ three `f` five
    e3 f = show $ one # two `f` (one # (one .+ one))
    e4 f = show $ two .+ three `f` (three .+ two)

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

-}

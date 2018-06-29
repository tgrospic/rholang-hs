module Numerals.ArabicNumeralsImpl
  ( eval1
  , isStructEq1
  , isEq1
  , AN (..)
  , eval2
  , isStructEq2
  , isEq2
  , AN3 (..)
  , eval3
  ) where

import Numerals.ArabicNumerals
import Data.Digits
import Data.MultiSet

-- Shallow embedding directly to Int

instance ArabicNumeralsSymantics Int where
  zero    = 0
  one     = 1
  two     = 2
  three   = 3
  a # b   = unDigits 10 [a, b]
  add a b = a + b

eval1 :: Int -> Int
eval1 = id

isStructEq1 :: Int -> Int -> Bool
isStructEq1 a b = a == b

isEq1 a b = eval1 a == eval1 b

-- isStructEq1 ≡ isEq1


-- With more structure we can have
-- structural equality for (#) but not for `add`

data AN = AN [Int] deriving (Show)

instance ArabicNumeralsSymantics AN where
  zero            = AN [0]
  one             = AN [1]
  two             = AN [2]
  three           = AN [3]
  (AN a) # (AN b) = AN $ a ++ b
  add a b         = AN [eval2 a + eval2 b]

eval2 (AN a) = unDigits 10 a

isStructEq2 :: AN -> AN -> Bool
isStructEq2 a b = trimZero a == trimZero b
  where
  -- Syntactic erasure: 00420 == 420
  trimZero (AN xs) = dropWhile (==0) xs

isEq2 a b = eval2 a == eval2 b

{-
> one # one `isStructEq1` add (one # zero) one
True
> one # one `isStructEq2` add (one # zero) one
False

> one # one `isEq1` add (one # zero) one
True
> one # one `isEq2` add (one # zero) one
True
-}


-- Next step is to add atructure for `add`

data AN3 = AN3 [Int] (MultiSet AN3) deriving (Eq, Ord, Show)

instance ArabicNumeralsSymantics AN3 where
  zero                    = AN3 [0] empty
  one                     = AN3 [1] empty
  two                     = AN3 [2] empty
  three                   = AN3 [3] empty
  (AN3 a as) # (AN3 b bs) = AN3 (a ++ b) (as `union` bs)
  add a b                 = AN3 [] (fromList [a, b])

eval3 (AN3 a as) = undefined

module Numerals2.ArabicSemantics where

import Data.Digits
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Numerals2.Numerals

data AN = AN [AN] | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9
          deriving (Eq, Show)

instance ArabicNumeralsSymantics AN where
  one   = A1; two   = A2; three = A3
  four  = A4; five  = A5; six   = A6
  seven = A7; eight = A8; nine  = A9

instance Monoid AN where
  mempty = A0

instance Semigroup AN where
  -- Zero erasure
  A0   <> a    = a
  -- Structural equivalance
  AN a <> AN b = AN $ a <> b
  AN a <> b    = AN $ a <> [b]
  a    <> AN b = AN $ a : b
  a    <> b    = AN [a, b]

arabicInts :: AN -> [Int]
arabicInts (AN a) = a >>= arabicInts
arabicInts a      = [fromJust $ elemIndex a arabicValues] where
  arabicValues = [A0, A1, A2, A3, A4, A5, A6, A7, A8, A9]

evalAN :: AN -> Int
evalAN = unDigits 10 . arabicInts

showAN :: AN -> String
showAN = foldl1 (<>) . (<$>) show . arabicInts

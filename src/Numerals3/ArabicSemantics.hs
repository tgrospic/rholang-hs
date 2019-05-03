{-# LANGUAGE FlexibleInstances #-}

module Numerals3.ArabicSemantics where

import Data.Digits
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Numerals3.Numerals

data ArabicDigit = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9
                   deriving (Eq, Show)

type AN = Calc ArabicDigit []

instance ArabicNumeralsSymantics AN where
  one   = Gen A1; two   = Gen A2; three = Gen A3
  four  = Gen A4; five  = Gen A5; six   = Gen A6
  seven = Gen A7; eight = Gen A8; nine  = Gen A9

instance Monoid AN where
  mempty = Gen A0

instance Semigroup AN where
  -- Zero erasure
  Gen A0 <> a = a

  -- Structural equivalance
  Rec a <> Rec b = Rec $ a <> b
  Rec a <> b     = Rec $ a <> [b]
  a     <> Rec b = Rec $ a : b
  a     <> b     = Rec [a, b]

arabicVal :: ArabicDigit -> Int
arabicVal a = index where
  index = fromJust $ elemIndex a arabicValues
  arabicValues = [A0, A1, A2, A3, A4, A5, A6, A7, A8, A9]

evalAN :: AN -> Int
evalAN (Rec a)  = unDigits 10 $ foldl f [] (reverse a) where
  f acc a = evalAN a : acc
evalAN (Gen a) = arabicVal a

showAN :: AN -> String
showAN (Rec a)  = foldl f "" a where
  f acc x = acc <> showAN x
showAN (Gen a) = show . arabicVal $ a

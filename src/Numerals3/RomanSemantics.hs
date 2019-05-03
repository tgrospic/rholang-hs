{-# LANGUAGE FlexibleInstances #-}

module Numerals3.RomanSemantics where

import Data.List (elemIndex)
import Data.Map (Map, fromList, (!?))
import Data.Maybe (fromJust)
import Numerals3.Numerals

data RomanDigit = RI | RV | RX | RL | RC | RD | RM deriving (Eq, Ord, Show)

type RN = Calc RomanDigit []

instance RomanNumeralsSymantics RN where
  _I = Gen RI
  _V = Gen RV
  _X = Gen RX
  _L = Gen RL
  _C = Gen RC
  _D = Gen RD
  _M = Gen RM

instance Semigroup RN where
  -- Structural equivalance
  Rec a <> Rec b = Rec $ a <> b
  Rec a <> b     = Rec $ a <> [b]
  a     <> Rec b = Rec $ a : b
  a     <> b     = Rec [a, b]

romanVal :: RomanDigit -> (Int, String)
romanVal = fromJust . (!?) romanValues where
  romanValues = fromList [
      (RI, (  1, "I")), (RV, (  5, "V")), (RX, (  10, "X")), (RL, (50, "L")),
      (RC, (100, "C")), (RD, (500, "D")), (RM, (1000, "M"))
    ]

evalRN :: RN -> Int
evalRN (Rec a) = fst $ foldl f (0, 0) (reverse a) where
  f (acc, last) a = case evalRN a of
    v | v < last -> (acc-v, v)
    v            -> (acc+v, v)
evalRN (Gen a) = fst . romanVal $ a

showRN :: RN -> String
showRN (Rec a) = foldl f "" a where
  f acc x = acc <> showRN x
showRN (Gen a) = snd . romanVal $ a

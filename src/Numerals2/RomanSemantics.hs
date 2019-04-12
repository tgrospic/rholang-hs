module Numerals2.RomanSemantics where

import Data.List (elemIndex)
import Data.Map (Map, fromList, (!?))
import Data.Maybe (fromJust)
import Numerals2.Numerals

data RN = RN [RN] | RI | RV | RX | RL | RC | RD | RM deriving (Eq, Ord, Show)

instance RomanNumeralsSymantics RN where
  _I = RI
  _V = RV
  _X = RX
  _L = RL
  _C = RC
  _D = RD
  _M = RM

instance Semigroup RN where
  -- Structural equivalance
  RN a <> RN b = RN $ a <> b
  RN a <> b    = RN $ a <> [b]
  a    <> RN b = RN $ a : b
  a    <> b    = RN [a, b]

romanValues = fromList [
    (RI, (  1, "I")), (RV, (  5, "V")), (RX, (  10, "X")), (RL, (50, "L")),
    (RC, (100, "C")), (RD, (500, "D")), (RM, (1000, "M"))
  ]

romanVal :: RN -> (Int, String)
romanVal = fromJust . (!?) romanValues

evalRN :: RN -> Int
evalRN (RN a) = fst $ foldl f (0, 0) (reverse a) where
  f (acc, last) a = case evalRN a of
    v | v < last -> (acc-v, v)
    v            -> (acc+v, v)
evalRN a = fst . romanVal $ a

showRN :: RN -> String
showRN (RN a) = foldl f "" a where
  f acc x = acc <> showRN x
showRN a = snd . romanVal $ a

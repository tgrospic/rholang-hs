{-# LANGUAGE TypeFamilies, GADTs #-}

module Numerals3.Numerals where

{-
  Arabic numerals represented as basic digits and Monoid
  with zero and binary operation.
-}
class Monoid n => ArabicNumeralsSymantics n where
  one, two, three, four, five, six, seven, eight, nine :: n

{-
  Roman numerals represented as basic digits and Semigroup
  binary operation (N zero?).
-}
class Semigroup n => RomanNumeralsSymantics n where
  _I, _V, _X, _L, _C, _D, _M :: n

{-
  Addition semantics is just a Monoid.
-}
class Monoid n => AdditionSymantics n

class ArabicAddSymantics n where
  type ArabicGen n
  ar :: ArabicGen n -> n

class RomanAddSymantics n where
  type RomanGen n
  ro :: RomanGen n -> n

{-
  Computational calculus representation consist of:
  - Rec (recursive part) represents collection of terms
  - Gen represents terms generator
-}
data Calc t c where
  Rec :: c (Calc t c) -> Calc t c
  Gen :: t            -> Calc t c

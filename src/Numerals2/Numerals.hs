{-# LANGUAGE TypeFamilies #-}

module Numerals2.Numerals where

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
  Roman numerals represented as basic digits and Semigroup
  binary operation (N zero?).
-}
class ArabicAddSymantics n where
  type ArabicGen n
  ar :: ArabicGen n -> n

class RomanAddSymantics n where
  type RomanGen n
  ro :: RomanGen n -> n

class Monoid n => AdditionSymantics n

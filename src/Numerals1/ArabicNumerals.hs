module Numerals1.ArabicNumerals
  ( ArabicNumerals1Symantics (..)
  ) where

class ArabicNumerals1Symantics n where
  zero  :: n
  one   :: n
  two   :: n
  three :: n
  four  :: n
  five  :: n
  six   :: n
  seven :: n
  eight :: n
  nine  :: n
  (#)   :: n -> n -> n
  (.+)  :: n -> n -> n

{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Numerals2.AdditionSyntax where

import Control.Applicative (many, some, (<|>))
import Data.Functor (($>))
import Text.Parsec (ParseError, char, parse, (<?>))
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (chainl1, eof)
import Numerals2.Numerals
import Numerals2.ArabicSyntax
import Numerals2.RomanSyntax

type AdditionParser p =
  ( ArabicNumeralsSymantics (ArabicGen p)
  , RomanNumeralsSymantics (RomanGen p)
  , ArabicAddSymantics p
  , RomanAddSymantics p
  , AdditionSymantics p
  ) => Parser p

__ :: Parser String
__ = many $ char ' '

p1, p2, p3, p4 :: AdditionParser p
p1 = ar <$> pArabicDigits
p2 = ro <$> pRomanDigits
p3 = char '(' *> pAddition <* char ')'
p4 = p1 <|> p2 <|> p3

-- Addition parser with Arabic and Roman numerals
pAddition :: AdditionParser p
pAddition = (__ *> p4 <* __) `chainl1` (char '+' $> (<>))

run :: Parser a -> String -> Either ParseError a
run p = parse (p <* eof) ""

runUnsafe :: Parser a -> String -> a
runUnsafe p = either (error . show) id . run p

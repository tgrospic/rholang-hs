{-# LANGUAGE Rank2Types #-}

module Numerals3.ArabicSyntax where

import Text.Parsec (char, (<?>))
import Text.Parsec.Combinator (choice, chainl1)
import Text.Parsec.String (Parser)
import Numerals3.Numerals

type ArabicNumeralsParser p = ArabicNumeralsSymantics p => Parser p

pDigit :: ArabicNumeralsParser p
pDigit = choice [f <$ char c | (f, c) <- terms] <?> "number" where
  terms = [(mempty, '0'), (one, '1'), (two,   '2'), (three, '3'), (four, '4')
          ,(five,   '5') ,(six, '6'), (seven, '7'), (eight, '8'), (nine, '9')
          ]

pArabicDigits :: ArabicNumeralsParser p
pArabicDigits = pDigit `chainl1` pure (<>)

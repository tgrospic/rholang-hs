{-# LANGUAGE Rank2Types #-}

module Numerals3.RomanSyntax where

import Text.Parsec (char, (<?>))
import Text.Parsec.Combinator (choice, chainl1)
import Text.Parsec.String (Parser)
import Numerals3.Numerals

type RomanNumeralsParser p = RomanNumeralsSymantics p => Parser p

p_ivxlcdm :: RomanNumeralsParser p
p_ivxlcdm = choice [f <$ char c | (f, c) <- terms] <?> "one of 'IVXLCDM" where
  terms = [(_I, 'I'), (_V, 'V'), (_X, 'X'), (_L, 'L'), (_C, 'C'), (_D, 'D'), (_M, 'M')]

pRomanDigits :: RomanNumeralsParser p
pRomanDigits = p_ivxlcdm `chainl1` pure (<>)

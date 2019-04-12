{-# LANGUAGE Rank2Types #-}

module Numerals2.RomanSyntax where

import Text.Parsec (char, (<?>))
import Text.Parsec.Combinator (choice, chainl1)
import Text.Parsec.String (Parser)
import Numerals2.Numerals

type RomanNumeralsParser p = RomanNumeralsSymantics p => Parser p

pI :: RomanNumeralsParser p
pI = _I <$ char 'I'

pV :: RomanNumeralsParser p
pV = _V <$ char 'V'

pX :: RomanNumeralsParser p
pX = _X <$ char 'X'

pL :: RomanNumeralsParser p
pL = _L <$ char 'L'

pC :: RomanNumeralsParser p
pC = _C <$ char 'C'

pD :: RomanNumeralsParser p
pD = _D <$ char 'D'

pM :: RomanNumeralsParser p
pM = _M <$ char 'M'

p_ivxlcdm :: RomanNumeralsParser p
p_ivxlcdm = choice [pI, pV, pX, pL, pC, pD, pM] <?> "one of 'IVXLCDM"

pRomanDigits :: RomanNumeralsParser p
pRomanDigits = p_ivxlcdm `chainl1` pure (<>)

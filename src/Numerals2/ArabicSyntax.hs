{-# LANGUAGE Rank2Types #-}

module Numerals2.ArabicSyntax where

import Text.Parsec (char, (<?>))
import Text.Parsec.Combinator (choice, chainl1)
import Text.Parsec.String (Parser)
import Numerals2.Numerals

type ArabicNumeralsParser p = ArabicNumeralsSymantics p => Parser p

pZero :: ArabicNumeralsParser p
pZero = mempty <$ char '0'

pOne :: ArabicNumeralsParser p
pOne = one <$ char '1'

pTwo :: ArabicNumeralsParser p
pTwo = two <$ char '2'

pThree :: ArabicNumeralsParser p
pThree = three <$ char '3'

pFour :: ArabicNumeralsParser p
pFour = four <$ char '4'

pFive :: ArabicNumeralsParser p
pFive = five <$ char '5'

pSix :: ArabicNumeralsParser p
pSix = six <$ char '6'

pSeven :: ArabicNumeralsParser p
pSeven = seven <$ char '7'

pEight :: ArabicNumeralsParser p
pEight = eight <$ char '8'

pNine :: ArabicNumeralsParser p
pNine = nine <$ char '9'

pDigit :: ArabicNumeralsParser p
pDigit = choice [pZero, pOne, pTwo, pThree, pFour, pFive, pSix, pSeven, pEight, pNine] <?> "number"

pArabicDigits :: ArabicNumeralsParser p
pArabicDigits = pDigit `chainl1` pure (<>)

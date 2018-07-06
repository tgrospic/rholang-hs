{-# LANGUAGE RankNTypes #-}

module Numerals.ArabicNumeralsParser
  (
    pdigits
  , pplus
  , arabicParser
  , run
  ) where

import Control.Applicative (many, some, (<|>))
import Control.Monad (replicateM)
import Data.Functor.Identity (Identity)
import Data.Monoid ((<>))
import Text.Parsec (Parsec, ParseError, anyChar, letter, digit, char, parse, try)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser, makeTokenParser, natural)
import Text.Parsec.Combinator (between, sepBy, sepBy1, choice, chainl1)
import Numerals.ArabicNumerals
import Numerals.ArabicNumeralsImpl

type ArabicParser p = ArabicNumeralsSymantics p => Parser p

__ = many $ char ' '

spaced p = __ *> p <* __

parens = between (spaced $ char '(') (spaced $ char ')')

pzero :: ArabicParser p
pzero = zero <$ char '0'

pone :: ArabicParser p
pone = one <$ char '1'

ptwo :: ArabicParser p
ptwo = two <$ char '2'

pthree :: ArabicParser p
pthree = three <$ char '3'

pfour :: ArabicParser p
pfour = four <$ char '4'

pfive :: ArabicParser p
pfive = five <$ char '5'

psix :: ArabicParser p
psix = six <$ char '6'

pseven :: ArabicParser p
pseven = seven <$ char '7'

peight :: ArabicParser p
peight = eight <$ char '8'

pnine :: ArabicParser p
pnine = nine <$ char '9'

pdigit :: ArabicParser p
pdigit = choice [pzero, pone, ptwo, pthree, pfour, pfive, psix, pseven, peight, pnine]

pdigits :: ArabicParser p
pdigits = foldl1 (#) <$> some pdigit

pplus :: ArabicParser p
pplus = arabicParser `chainl1` do{ char '+' <* __; pure (.+) }

-- must have top parens: (23+123)
arabicParser :: ArabicParser p
arabicParser = parens pplus <|> spaced pdigits

run :: Parser a -> String -> a
run p inp = res $ parse p "" inp
  where
  res (Left ex) = error "Parse error"
  res (Right res) = res

-- result = "(((2+4)+52)+342)"
result = show4 $ run arabicParser "( 2 + 4 +52+(342) )"

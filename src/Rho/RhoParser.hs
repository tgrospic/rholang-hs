module Rho.RhoParser where

import Control.Applicative (many, some, (<|>))
import Control.Monad (replicateM)
import Data.Functor.Identity (Identity)
import Data.Monoid ((<>))
import Text.Parsec (Parsec, ParseError, anyChar, letter, digit, char, parse)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser, makeTokenParser, natural)

tokenParser :: GenTokenParser String u Identity
tokenParser = makeTokenParser haskellDef

pnumber :: Parser Int
pnumber = fromInteger <$> natural tokenParser

anyString :: Int -> Parser String
anyString x = replicateM x anyChar

varChar :: Parser Char
varChar = letter <|> digit <|> char '_' <|> char '\''

-- token Var (((letter | '\'') (letter | digit | '_' | '\'')*)|(('_') (letter | digit | '_' | '\'')+))
var :: Parser String
var = (:) <$> (letter <|> char '\'') <*> many varChar
 <|>  (:) <$>             char '_'   <*> some varChar

-- https://github.com/rchain/rchain/blob/master/rholang/src/main/bnfc/rholang_mercury.cf

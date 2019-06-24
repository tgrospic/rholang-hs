{-# LANGUAGE GADTs #-}

module Rholang4b.Syntax where

import Control.Applicative (many, some, (<|>), liftA2)
import Data.Functor.Identity (Identity)
import Text.Parsec (Parsec, ParseError, anyChar, letter, digit, char, string, parse, try)
import Text.Parsec.Combinator (between, sepBy, sepBy1, choice, chainl1)
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Rholang4b.Rholang
import Rholang4b.Print
import qualified Data.MultiSet as MS

-- Rholang syntax

-- https://github.com/rchain/rchain/blob/master/rholang/src/main/bnfc/rholang_mercury.cf

tokenParser :: P.GenTokenParser String u Identity
tokenParser = P.makeTokenParser javaStyle

-- Comments are included in whitespace
__ :: Parser ()
__ = P.whiteSpace tokenParser

varChar :: Parser Char
varChar = letter <|> digit <|> char '_' <|> char '\''

-- token Var (((letter | '\'') (letter | digit | '_' | '\'')*)|(('_') (letter | digit | '_' | '\'')+))
var :: Parser String
var = (:) <$> (letter <|> char '\'') <*> many varChar
  <|> (:) <$>             char '_'   <*> some varChar

pNil :: Parser Process
pNil = Stop <$ string "Nil"

pTrue :: Parser Process
pTrue = (Gnd $ GBool True) <$ string "true"

pFalse :: Parser Process
pFalse = (Gnd $ GBool False) <$ string "false"

pString :: Parser Process
pString = Gnd . GString <$> P.stringLiteral tokenParser

pInt :: Parser Process
pInt = Gnd . GInt <$> P.integer tokenParser

pVar :: Parser Process
pVar = Var <$> var

-- Input
pFor :: Parser Process
pFor = do
  _ <- string "for" *> __ *> char '('
  y <- __ *> pPar
  _ <- __ *> string "<-"
  x <- __ *> pPar
  _ <- __ *> char ')'
  p <- __ *> char '{' *> pPar <* char '}'
  pure $ Input y x p

-- Output
pOut :: Parser Process
pOut = do
  -- n <- pPar <* __ <* char '!'
  -- Without syntax distinction between processes and names
  -- we must be careful here not to create infinite loop or
  -- grammar with left recursion. To solve this problem Par
  -- terms (non-terminals) must be within braces.
  n <- (pProc1 <|> pVar <|> pProc4) <* __ <* char '!'
  p <- __ *> char '(' *> pPar <* char ')'
  pure $ Output n p

-- Process
pProc1, pProc2, pProc3, pProc4, pProc :: Parser Process
pProc1 = pString <|> pInt <|> pNil <|> pFalse <|> pTrue
pProc2 = try pOut <|> try pFor <|> try pProc1 <|> pVar

pProc3 = __ *> pProc2 <* __
pProc4 = __ *> char '{' *> pPar <* char '}' <* __

pProc = try pProc3 <|> pProc4

-- Rho parser
pPar :: Parser Process
pPar = pProc `chainl1` (char '|' *> pure mkPar) where
  mkPar p q = Par $ MS.fromList [p, q]

rhoParse :: Parser a -> String -> Either ParseError a
rhoParse p = parse p ""

run :: Parser a -> String -> a
run p inp = res $ rhoParse p inp where
  res (Left ex) = error $ show ex
  res (Right a) = a

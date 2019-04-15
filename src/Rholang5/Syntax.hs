{-# LANGUAGE Rank2Types #-}

module Rholang5.Syntax where

import Control.Applicative (many, some, (<|>), liftA2)
import Data.Functor.Identity (Identity)
import Text.Parsec (Parsec, ParseError, anyChar, letter, digit, char, string, parse, try, eof)
import Text.Parsec.Combinator (between, sepBy, sepBy1, choice, chainl1)
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Rholang5.RhoFinal

-- Syntax

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

type RholangParser b a = (RholangSymantics a) => Parser (a b)

pNil :: RholangParser P a
pNil = nil <$ string "Nil"

pTrue :: RholangParser P a
pTrue = eval (gBool True) <$ string "true"

pFalse :: RholangParser P a
pFalse = eval (gBool False) <$ string "false"

pString :: RholangParser P a
pString = eval . gStr <$> P.stringLiteral tokenParser

pInt :: RholangParser P a
pInt = eval . gInt <$> P.integer tokenParser

pVarName :: RholangParser N a
pVarName = nVar <$> var

pVarProc  :: RholangParser P a
pVarProc = pVar <$> var

-- Input
pFor :: RholangParser P a
pFor = do
  _ <- string "for" *> __ *> char '('
  y <- __ *> pName
  _ <- __ *> string "<-"
  x <- __ *> pName
  _ <- __ *> char ')'
  p <- __ *> char '{' *> pPar <* char '}'
  pure $ for y x p

-- Output
pOut :: RholangParser P a
pOut = do
  n <- pName <* __ <* char '!'
  p <- __ *> char '(' *> pPar <* char ')'
  pure $ out n p

-- Name
pName :: RholangParser N a
pName = try quotedVar <|> try quotedProc <|> variable where
  quotedVar  = quo <$> (char '@' *> pVarProc)
  quotedProc = quo <$> (char '@' *> pPar)
  variable   = pVarName

-- Dereference
pEval :: RholangParser P a
pEval = eval <$> (char '*' *> pName)

-- Process
pProc1, pProc2, pProc3, pProc4, pProc :: RholangParser P a
pProc1 = pString <|> pInt <|> pNil <|> pFalse <|> pTrue
pProc2 = pEval <|> try pProc1 <|> try pOut <|> pFor <|> pVarProc

pProc3 = __ *> pProc2 <* __
pProc4 = __ *> char '{' *> pPar <* char '}' <* __

pProc = try pProc3 <|> pProc4

-- Rho parser
pPar :: RholangParser P a
pPar = pProc `chainl1` (char '|' *> pure par)

run :: Parser a -> String -> Either ParseError a
run p = parse (p <* eof) ""

runUnsafe :: Parser a -> String -> a
runUnsafe p = either (error . show) id . run p

rhoParse :: RholangSymantics a => String -> Either ParseError (a P)
rhoParse = run pPar

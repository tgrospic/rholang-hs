{-# LANGUAGE GADTs #-}

module Rholang4.Syntax where

import Control.Applicative (many, some, (<|>), liftA2)
import Data.Functor.Identity (Identity)
import Text.Parsec (Parsec, ParseError, anyChar, letter, digit, char, string, parse, try)
import Text.Parsec.Combinator (between, sepBy, sepBy1, choice, chainl1)
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Rholang4.Rholang
import Rholang4.Print
import qualified Data.MultiSet as MS

-- Rholang syntax

-- Syntax erasure *@P = P
eval (Quote p) = p
eval n         = Eval n

-- Syntax erasure @*P == P
quo (Eval n) = n
quo p        = Quote p

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
pTrue = (eval $ Gnd P $ GBool True) <$ string "true"

pFalse :: Parser Process
pFalse = (eval $ Gnd P $ GBool False) <$ string "false"

pString :: Parser Process
pString = eval . Gnd P . GString <$> P.stringLiteral tokenParser

pInt :: Parser Process
pInt = eval . Gnd P . GInt <$> P.integer tokenParser

pVarName :: Parser Name
pVarName = Var N <$> var

-- Process variable is encoded as reified `*(Var P)`
pVarProc  :: Parser Process
pVarProc = eval . Var P <$> var

-- Input
pFor :: Parser Process
pFor = do
  _ <- string "for" *> __ *> char '('
  y <- __ *> pName
  _ <- __ *> string "<-"
  x <- __ *> pName
  _ <- __ *> char ')'
  p <- __ *> char '{' *> pPar <* char '}'
  pure $ Input y x p

-- Output
pOut :: Parser Process
pOut = do
  n <- pName <* __ <* char '!'
  p <- __ *> char '(' *> pPar <* char ')'
  pure $ Output n p

-- Name
pName :: Parser Name
pName = try quotedVar <|> try quotedProc <|> variable
  where
    quotedVar  = quo <$> (char '@' *> pVarProc)
    quotedProc = quo <$> (char '@' *> pPar)
    variable   = pVarName

-- Dereference
pEval :: Parser Process
pEval = Eval <$> (char '*' *> pName)

-- Process
pProc1, pProc2, pProc3, pProc4, pProc :: Parser Process
pProc1 = pString <|> pInt <|> pNil <|> pFalse <|> pTrue
pProc2 = pEval <|> try pProc1 <|> try pOut <|> pFor <|> pVarProc

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
run p inp = res $ rhoParse p inp
  where
  res (Left ex) = error $ show ex
  res (Right a) = a

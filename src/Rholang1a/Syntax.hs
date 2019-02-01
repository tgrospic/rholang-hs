module Rholang1a.Syntax where

import Control.Applicative (many, some, (<|>), liftA2)
import Data.Functor.Identity (Identity)
import Text.Parsec (Parsec, ParseError, anyChar, letter, digit, char, string, parse, try)
import Text.Parsec.Combinator (between, sepBy, sepBy1, choice, chainl1)
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Rholang1a.RhoFinal
import Rholang1a.RhoInitial
import Rholang1a.Semantics

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

pNil :: ProcessSymantics p => Parser p
pNil = nil <$ string "Nil"

pTrue :: ProcessSymantics p => Parser p
pTrue = (eval $ lit (LBool True)) <$ string "true"

pFalse :: ProcessSymantics p => Parser p
pFalse = (eval $ lit (LBool False)) <$ string "false"

pString :: ProcessSymantics p => Parser p
pString = eval . lit . LString <$> P.stringLiteral tokenParser

pInt :: ProcessSymantics p => Parser p
pInt = eval . lit . LInt <$> P.integer tokenParser

pVarName :: NameSymantics n => Parser n
pVarName = lit . LVar <$> var

pVarProc  :: ProcessSymantics p => Parser p
pVarProc = eval <$> pVarName

-- Inputs
pFor :: ProcessSymantics p => Parser p
pFor = do
  _ <- string "for" *> __ *> char '('
  y <- __ *> pName
  _ <- __ *> string "<-"
  x <- __ *> pName
  _ <- __ *> char ')'
  p <- __ *> char '{' *> pPar <* char '}'
  pure $ for y x p

-- Outputs
pOut :: ProcessSymantics p => Parser p
pOut = do
  n <- pName <* __ <* char '!'
  p <- __ *> char '(' *> pPar <* char ')'
  pure $ out n p

-- Name
pName :: NameSymantics n => Parser n
pName = try (quo <$> (char '@' *> pVarProc)) <|> try (quo <$> (char '@' *> pPar)) <|> pVarName
-- This does not compile, if defined in where block ??
-- pName = try quotedVar <|> try quotedProc <|> variable
--   where
--     quotedVar  = quo <$> (char '@' *> pVarProc)
--     quotedProc = quo <$> (char '@' *> pPar)
--     variable   = lit <$> pVarName

-- Dereference
pEval :: ProcessSymantics p => Parser p
pEval = eval <$> (char '*' *> pName)

-- Process
pProc1, pProc2, pProc3, pProc4, pProc :: ProcessSymantics p => Parser p
pProc1 = pString <|> pInt <|> pNil <|> pFalse <|> pTrue
pProc2 = pEval <|> try pProc1 <|> try pOut <|> pFor <|> try pOut <|> pVarProc

pProc3 = __ *> pProc2 <* __
pProc4 = __ *> char '{' *> pPar <* char '}' <* __

pProc = try pProc3 <|> pProc4

-- Rho parser
pPar :: ProcessSymantics p => Parser p
pPar = pProc `chainl1` (char '|' *> pure (.|))


run :: Parser a -> String -> Either String a
run p inp = res $ parse p "" inp
  where
  res (Left ex) = Left $ show ex
  res (Right a) = Right a

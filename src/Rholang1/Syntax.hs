module Rholang1.Syntax where

import Control.Applicative (many, some, (<|>), liftA2)
import Data.Functor.Identity (Identity)
import Text.Parsec (Parsec, ParseError, anyChar, letter, digit, char, string, parse, try)
import Text.Parsec.Combinator (between, sepBy, sepBy1, choice, chainl1)
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Rholang1.RhoFinal
import Rholang1.RhoInitial
import Rholang1.Semantics
import Rholang1.Print

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

pNil :: Parser Process
pNil = nil <$ string "Nil"

pTrue :: Parser Process
pTrue = (eval $ lit (LBool True)) <$ string "true"

pFalse :: Parser Process
pFalse = (eval $ lit (LBool False)) <$ string "false"

pString :: Parser Process
pString = eval . lit . LString <$> P.stringLiteral tokenParser

pInt :: Parser Process
pInt = eval . lit . LInt <$> P.integer tokenParser

pVarName :: Parser Name
pVarName = lit . LVar <$> var

pVarProc  :: Parser Process
pVarProc = eval <$> pVarName

-- Inputs
pFor :: Parser Process
pFor = do
  _ <- string "for" *> __ *> char '('
  y <- __ *> pName
  _ <- __ *> string "<-"
  x <- __ *> pName
  _ <- __ *> char ')'
  p <- __ *> char '{' *> pPar <* char '}'
  pure $ for y x p

-- Outputs
pOut :: Parser Process
pOut = do
  n <- pName <* __ <* char '!'
  p <- __ *> char '(' *> pPar <* char ')'
  pure $ out n p

-- Name
pName :: Parser Name
pName = try quotedVar <|> try quotedProc <|> variable
  where
    quotedVar  = quo <$> (char '@' *> pVarProc)
    quotedProc = quo <$> (char '@' *> pPar)
    variable   = pVarName

-- Dereference
pEval :: Parser Process
pEval = eval <$> (char '*' *> pName)

-- Process
pProc1, pProc2, pProc3, pProc4, pProc :: Parser Process
pProc1 = pString <|> pInt <|> pNil <|> pFalse <|> pTrue
pProc2 = pEval <|> try pProc1 <|> try pOut <|> pFor <|> try pOut <|> pVarProc

pProc3 = __ *> pProc2 <* __
pProc4 = __ *> char '{' *> pPar <* char '}' <* __

pProc = try pProc3 <|> pProc4

-- Rho parser
pPar :: Parser Process
pPar = pProc `chainl1` (char '|' *> pure (.|))


run :: Parser a -> String -> Either String a
run p inp = res $ parse p "" inp
  where
  res (Left ex) = Left $ show ex
  res (Right a) = Right a

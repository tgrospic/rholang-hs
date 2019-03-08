{-# LANGUAGE GADTs #-}

module Rholang4a.Print where

import Control.Monad (join)
import Data.List (intercalate, replicate)
import qualified Data.MultiSet as MS
import Rholang4a.Rholang

instance Show Ground where
  show (GBool   x) = show x
  show (GInt    x) = show x
  show (GString x) = show x
  show (GUri    x) = "Uri(" <> show x <> ")"
  show (GUnforg x) = "Unforg(" <> show x <> ")"

quoted p = "@{" <> p <> "}"

instance Show Process where
  -- Names
  show (Quote p)   = quoted $ show p
  show (Bound _ i) = 'x' : show i
  show (Var   _ n) = n
  show (Gnd   _ g) = show g
  -- Processes
  show p = showIndent 0 p

-- Print with indentation

showIndent :: Int -> Process -> String
showIndent i Stop = "Nil"
-- Print input
showIndent i (Input y x p) =
  "for( " <> showBound y <> " <- " <> showBound x <> " ) {\n" <>
    indent (i+1) <> showIndent (i+1) p <> "\n" <>
    indent i <> "}" where
  -- Indentation space
  indent i = join (replicate i "  ")
-- Print output
showIndent i (Output x p) = showBound x <> "!(" <> show p <> ")"
-- Print drop
-- Process variable is encoded as reified `*(Var P)`
showIndent i (Eval n@(Bound P _)) = show n
showIndent i (Eval n@(Gnd   P _)) = show n
showIndent i (Eval n@(Var   P _)) = show n
showIndent i (Eval n)             = '*' : show n
-- Print Par
showIndent i (Par ms) = intercalate " | " $ showPar i <$> MS.toOccurList ms where
  showPar i (p, n) = join $ replicate n $ showIndent i p

-- Bound Process variable from pattern `for( ... <- @{x} )`
-- Bound Process variable from pattern `@{x}!(...)`
showBound n@(Bound P _) = quoted $ show n
showBound n@(Gnd   P _) = quoted $ show n
showBound n@(Var   P _) = quoted $ show n
showBound n             = show n

{-# LANGUAGE GADTs #-}

module Rholang4b.Print where

import Control.Monad (join)
import Data.List (intercalate, replicate)
import qualified Data.MultiSet as MS
import Rholang4b.Rholang

instance Show Ground where
  show (GBool   x) = show x
  show (GInt    x) = show x
  show (GString x) = show x
  show (GUri    x) = "Uri(" <> show x <> ")"
  show (GUnforg x) = "Unforg(" <> show x <> ")"

instance Show Process where
  -- Variables
  show (Bound i) = 'x' : show i
  show (Var   n) = n
  show (Gnd   g) = show g
  -- Processes
  show p = showIndent 0 p

-- Print with indentation

showIndent :: Int -> Process -> String
showIndent i Stop = "Nil"
-- Print input
showIndent i (Input y x p) =
  "for( " <> showProc y <> " <- " <> showProc x <> " ) {\n" <>
    indent (i+1) <> showIndent (i+1) p <> "\n" <>
    indent i <> "}" where
  -- Indentation space
  indent i = join (replicate i "  ")
-- Print output
showIndent i (Output x p) = showProc x <> "!(" <> show p <> ")"
-- Print Par
showIndent i (Par ms) = pipeify $ showPar i <$> MS.toOccurList ms where
  showPar i (p, n) = pipeify $ replicate n $ showIndent i p
  pipeify = intercalate " | "
-- Variables
showIndent _ p = show p

-- Print the process within braces if it can be resursive
showProc n@Stop      = show n
showProc n@(Bound _) = show n
showProc n@(Var   _) = show n
showProc n@(Gnd   _) = show n
showProc n           = braces $ show n

braces p = "{" <> p <> "}"

{-# LANGUAGE TypeFamilies #-}

module Rholang5.Print where

import Control.Monad (join)
import Data.List (intercalate, replicate)
import qualified Data.MultiSet as MS
import Rholang5.RhoFinal
import Rholang5.RhoInitial
import Rholang5.Semantics

-- newtype RhoPrint a = RhoPrint { unRhoPrint :: RhoPrintT a }

-- type family RhoPrintT a
-- type instance RhoPrintT N = String
-- type instance RhoPrintT P = String

-- instance ProcessSymantics RhoPrint where
--   nil       = RhoPrint "Nil"
--   par a b   = RhoPrint "Par"
--   for y x p = RhoPrint "For"
--   out a b   = RhoPrint "Out"
--   eval n    = RhoPrint "Eval"

-- instance NameSymantics RhoPrint where
--   quo (RhoPrint p) = RhoPrint $ "@" <> p

-- instance VariableSymantics RhoPrint where
--   nvar v = RhoPrint $ "VAR_NAME: " <> v
--   pvar v = RhoPrint $ "VAR_PROC: " <> v

-- instance GroundNameSymantics RhoPrint where
--   gint n  = RhoPrint $ "Int: " <> show n
--   gstr s  = RhoPrint $ "String: " <> show s
--   gbool b = RhoPrint $ "Bool: " <> show b
--   guri x  = RhoPrint $ "Uri: " <> show x

instance Show Name where
  show (Name p)    = quoted $ show p
  show (Bound _ i) = 'x' : show i
  show (Var   _ n) = n
  show (Gnd   _ g) = show g

instance Show Ground where
  show (GBool   x) = show x
  show (GInt    x) = show x
  show (GString x) = show x
  show (GUri    x) = "Uri(" <> show x <> ")"
  show (GUnforg x) = "Unforg(" <> show x <> ")"

quoted p = "@{" <> p <> "}"

instance Show Process where
  show = showIndent 0

-- Print with indentation

showIndent :: Int -> Process -> String
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
showIndent i (Par ms) = if MS.null ms then "Nil" else parString where
  parString = intercalate " | " $ showPar i <$> MS.toOccurList ms
  showPar i (p, n) = join $ replicate n $ showIndent i p

-- Bound Process variable from pattern `for( ... <- @{x} )`
-- Bound Process variable from pattern `@{x}!(...)`
showBound n@(Bound P _) = quoted $ show n
showBound n@(Gnd   P _) = quoted $ show n
showBound n@(Var   P _) = quoted $ show n
showBound n             = show n

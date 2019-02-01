{-# LANGUAGE RecordWildCards #-}

module Rholang1.Print where

import Control.Monad (join)
import Data.List (intercalate, replicate)
import qualified Data.MultiSet as MS
import Rholang1.RhoFinal
import Rholang1.RhoInitial

-- Print

instance Show Process where
  show pr@Process{..} = if pr == procEmpty then "Nil" else showPar $ showPar <$> join pars
    where
      pars = [ showInputs  <$> MS.toOccurList inputs
             , showOutputs <$> MS.toOccurList outputs
             , showDrops   <$> MS.toOccurList drops
             ]
      showPar = intercalate " | "
      -- Print inputs
      showInputs  ((Lit n,       p), i) = replicate i $ "for( " <> ('x' : show (height-1)) <> " <- " <> show n <> " ) { " <> show p <> " }"
      showInputs  ((BoundName n, p), i) = replicate i $ "for( " <> ('x' : show (height-1)) <> " <- " <> ('x' : show n) <> " ) { " <> show p <> " }"
      showInputs  ((n,           p), i) = replicate i $ "for( " <> showName ("x" <> show (height-1)) <> " <- " <> showName (show n) <> " ) { " <> show p <> " }"
      -- Print outputs
      showOutputs ((Lit n, p), i) = replicate i $ "" <> show n <> "!(" <> show p <> ")"
      showOutputs ((n, p),     i) = replicate i $ "" <> showName (show n) <> "!(" <> show p <> ")"
      -- Print drops
      showDrops    (n,         i) = replicate i $ show n

showName p = "@{" <> p <> "}"

instance Show Name where
  show (Name p)      = show p
  show (BoundName i) = 'x' : show i
  show (Lit lit)     = show lit

instance Show Literal where
  show (LBool x)   = show x
  show (LInt x)    = show x
  show (LString x) = show x
  show (LVar x)    = x

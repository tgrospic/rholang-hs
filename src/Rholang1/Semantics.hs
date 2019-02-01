{-# LANGUAGE TypeFamilies, RecordWildCards #-}

module Rholang1.Semantics where

import qualified Data.MultiSet as MS
import Rholang1.RhoFinal
import Rholang1.RhoInitial

-- Rholang semantics

type instance TProcess Name = Process

type instance TName Process = Name

instance ProcessSymantics Process where
  nil  = procEmpty
  for y x p = procEmpty {
    -- substitute x for y in P `for( y <- x ) P`
    -- TODO: y cannot have holes e.g. `for( @{a | b} <- x )`
    inputs = MS.singleton (x, substitute y (BoundName $ height p) p),
    height = height p + 1
  }
  out n p = procEmpty {
    outputs = MS.singleton (n, p),
    height  = height p
  }
  p .| q = procEmpty {
    inputs  = MS.union (inputs p) (inputs q),
    outputs = MS.union (outputs p) (outputs q),
    drops   = MS.union (drops p) (drops q),
    height  = max (height p) (height q)
  }
  -- syntax erasure *@P = P
  eval (Name p) = p
  eval n = procEmpty { drops = MS.singleton n }

-- substitute x for y in p
substitute :: Name -> Name -> Process -> Process
substitute y x = sub
  where
    sub Process{..} = procEmpty {
      inputs  = MS.map f inputs,
      outputs = MS.map f outputs,
      drops   = MS.map g drops,
      height  = height
    }
    f (n, q) = (if n == y then x else n, substitute y x q)
    g n      = if n == y then x else n

instance NameSymantics Name where
  quo = Name
  lit = Lit

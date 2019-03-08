{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Rholang4a.Rholang where

import qualified Data.MultiSet as MS

-- RHO-calculus :: Deep embedding (collection)

data Process where
  Stop   :: Process
  Input  :: Process -> Process -> Process -> Process
  Output :: Process -> Process -> Process
  Eval   :: Process -> Process
  Par    :: MS.MultiSet Process -> Process
  -- Names
  Quote  :: Process -> Process
  -- Process (P) bound/variable/ground `@x!(x)` or `for( @y <- @x )`
  -- Name    (N) bound/variable/ground `x!(*x)` or `for( y <- x )`
  Bound  :: NP -> Integer -> Process
  Var    :: NP -> String  -> Process
  Gnd    :: NP -> Ground  -> Process

deriving instance Eq Process
deriving instance Ord Process

-- Distinction between process and name variables,
-- this is really only needed for printer to
-- keep distinction as defined in user code
data NP = N | P deriving (Eq, Ord, Show)

-- Built-in types
data Ground
  = GBool   Bool
  | GInt    Integer
  | GString String
  | GUri    String
  | GUnforg Integer
  deriving (Eq, Ord)

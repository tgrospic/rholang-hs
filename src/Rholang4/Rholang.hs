{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Rholang4.Rholang where

import qualified Data.MultiSet as MS

-- RHO-calculus :: Deep embedding (collection)

data RProcess
data RName

type Process = Rho RProcess
type Name    = Rho RName

data Rho p where
  Stop   :: Process
  Input  :: Name -> Name    -> Process -> Process
  Output :: Name -> Process -> Process
  Eval   :: Name -> Process
  Par    :: MS.MultiSet Process -> Process
  -- Names
  Quote  :: Process -> Name
  -- Process (P) bound/variable/ground `@x!(x)` or `for( @y <- @x )`
  -- Name    (N) bound/variable/ground `x!(*x)` or `for( y <- x )`
  Bound  :: NP -> Integer -> Name
  Var    :: NP -> String  -> Name
  Gnd    :: NP -> Ground  -> Name

deriving instance Eq (Rho a)
deriving instance Ord (Rho a)

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

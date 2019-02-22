module Rholang3.RhoInitial where

import qualified Data.MultiSet as MS

-- RHO-calculus :: Initial encoding (collection)

data Process
  = Stop
  | Input  Name Name    Process
  | Output Name Process
  | Eval   Name
  | Par    (MS.MultiSet Process)
  deriving (Eq, Ord)

-- Distinction between process and name variables,
-- this is really only needed for printer to
-- keep distinction as defined in user code
data NP = N | P deriving (Eq, Ord, Show)

data Name
  = Name  Process
  -- Process (P) bound/variable/ground `@x!(x)` or `for( @y <- @x )`
  -- Name    (N) bound/variable/ground `x!(*x)` or `for( y <- x )`
  | Bound NP Integer
  | Var   NP String
  | Gnd   NP Ground
  deriving (Eq, Ord)

-- Built-in types
data Ground
  = GBool   Bool
  | GInt    Integer
  | GString String
  | GUri    String
  | GUnforg Integer
  deriving (Eq, Ord)

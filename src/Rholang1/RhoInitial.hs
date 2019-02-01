module Rholang1.RhoInitial where

import qualified Data.MultiSet as MS
import Rholang1.RhoFinal

-- RHO-calculus :: Initial encoding (collection)

data Process = Process
  { inputs  :: MS.MultiSet (Name, Process)
  , outputs :: MS.MultiSet (Name, Process)
  , drops   :: MS.MultiSet Name
  , height  :: Integer
  }
  deriving (Eq, Ord)

data Name
  = Name Process
  | BoundName Integer
  | Lit Literal
  deriving (Eq, Ord)

-- data Literal
--   = LBool Bool
--   | LInt Integer
--   | LString String
--   | LVar String
--   deriving (Eq, Ord)

-- Nil process, par's unit value
procEmpty = Process
  { inputs  = MS.empty
  , outputs = MS.empty
  , drops   = MS.empty
  , height  = 0
  }

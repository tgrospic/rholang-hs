module Rholang2.RhoInitial where

import qualified Data.MultiSet as MS
import Rholang2.RhoFinal

-- RHO-calculus :: Initial encoding (collection)

data Process = Process
  { input  :: Maybe (Bool, Name, Process)
  , output :: Maybe (Name, Process)
  , reify  :: Maybe Name
  , par    :: MS.MultiSet Process
  , height :: Integer
  }
  deriving (Eq, Ord)

data Name
  = Name      Process
  | BoundName Integer -- pattern bound name `for( x <- ... )`
  | BoundProc Integer -- pattern bound name `for( @x <- ... )`
  | VarProc   String  -- Process variable `@x!(x)` or `for( ... <- @x )`
  | VarName   String  -- Name variable    `x!(*x)` or `for( ... <- x )`
  | GndP      Ground  -- Ground process (encoded as reified name)
  | GndN      Ground  -- Ground name (encoded as reified name)
  deriving (Eq, Ord)

-- Built-in types
data Ground
  = GBool   Bool
  | GInt    Integer
  | GString String
  | GUri    String
  | GUnforg Integer
  deriving (Eq, Ord)

-- Nil process, par's unit value
procEmpty  = Process
  { input  = Nothing
  , output = Nothing
  , reify  = Nothing
  , par    = MS.empty
  , height = 0
  }

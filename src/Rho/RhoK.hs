{-# LANGUAGE TypeFamilies #-}

-- This implementation follows original version by L.G.Meredith
-- https://github.com/AkbarsGrasp/RhoMachine/blob/9ac6b0af08d6272638314eb9d850392996f2ef8f/RhoCalc.lhs

module Rho.RhoK where

type family TProcess name

type family TName process

type family TContext process

class ProcessSymantics p where
  -- 0 // nil or stopped process
  nil  :: p
  -- for( y <- x ) P // input guarded process
  for  :: TName p -> TName p -> p -> p
  -- x!( Q ) // output
  out  :: TName p -> p -> p
  -- P|Q // parallel composition
  (.|) :: p -> p -> p
  -- *x // dereferenced or unquoted name
  eval :: TName p -> p
  -- location update
  update :: TName p -> p
  -- situation catalyst
  comm :: TContext p -> p

class NameSymantics n where
  -- @P // name or quoted process
  quo :: (TContext n, TProcess n) -> n

class ContextSymantics k where
  -- []
  hole :: k
  -- for( y <- x ) K
  forK :: TName k -> TName k -> k -> k
  -- x!( K )
  outK :: TName k -> k -> k
  -- P|K
  parK :: TProcess k -> k -> k

data Process
  = Stop
  | Input  Name    Name    Process
  | Output Name    Process
  | Par    Process Process
  | Eval   Name
  | Update Name
  | Comm   Context
  deriving (Eq, Show)

data Name = Name (Context, Process)
            | Address Int
            deriving (Eq, Show)

data Context
  = Hole
  | InputK  Name    Name    Context
  | OutputK Name    Context
  | ParK    Process Context
  deriving (Eq, Show)

type instance TProcess Name    = Process
type instance TProcess Context = Process

type instance TName Process = Name
type instance TName Context = Name

type instance TContext Process = Context
type instance TContext Name    = Context

instance ProcessSymantics Process where
  nil    = Stop
  for    = Input
  out    = Output
  (.|)   = Par
  eval   = Eval
  update = Update
  comm   = Comm

instance NameSymantics Name where
  quo  = Name

instance ContextSymantics Context where
  hole = Hole
  forK = InputK
  outK = OutputK
  parK = ParK

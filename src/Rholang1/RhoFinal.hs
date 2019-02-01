{-# LANGUAGE TypeFamilies #-}

module Rholang1.RhoFinal where

-- RHO-calculus :: Final encoding (terms)

type family TName p

type family TProcess n

class ProcessSymantics p where
  -- 0 // nil or stopped process
  nil  :: p
  -- P|Q // parallel composition
  (.|) :: p -> p -> p
  -- for( y <- x ) P // input guarded process
  for  :: TName p -> TName p -> p -> p
  -- x!( Q ) // output
  out  :: TName p -> p -> p
  -- *x // dereferenced or unquoted name
  eval :: TName p -> p

class NameSymantics n where
  -- @P // name or quoted process
  quo :: TProcess n -> n
  -- variable or new name
  -- lit :: n -> n
  lit :: Literal -> n

data Literal
  = LBool Bool
  | LInt Integer
  | LString String
  | LVar String
  deriving (Eq, Ord)

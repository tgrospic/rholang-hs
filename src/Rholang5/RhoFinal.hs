{-# LANGUAGE TypeFamilies, Rank2Types, FlexibleInstances #-}

module Rholang5.RhoFinal where

-- Rholang syntax-semantics interface

data P; data N

class ProcessSymantics p where
  -- 0 // nil or stopped process
  nil  :: p P
  -- P|Q // parallel composition
  par  :: p P -> p P -> p P
  -- for( y <- x ) P // input guarded process
  for  :: p N -> p N -> p P -> p P
  -- x!( Q ) // output
  out  :: p N -> p P -> p P
  -- *x // dereferenced or unquoted name
  eval :: p N -> p P

class NameSymantics n where
  -- @P // name or quoted process
  quo  :: n P -> n N

-- Variable symantics
class VariableSymantics n where
  -- Variable (name/process)
  nvar   :: String -> n N
  pvar   :: String -> n P

-- Ground terms
class GroundNameSymantics n where
  gint  :: Integer -> n N
  gstr  :: String  -> n N
  gbool :: Bool    -> n N
  guri  :: String  -> n N

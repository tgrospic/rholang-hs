module Rholang
    ( RhoProc (..)
    , RhoName (..)
    ) where

newtype RhoName a = RhoName a deriving (Eq, Ord, Show)

class RhoProc r where
  -- 0 // nil or stopped process
  nil :: r a
  -- for( y <- x ) P // input guarded process
  for :: (Ord a, Show a) => a -> a -> r a -> r a
  -- x!( @Q ) // output
  out :: (Ord a, Show a) => a -> r (RhoName a) -> r a
  -- P|Q // parallel composition
  (.|) :: (Ord a) => r a -> r a -> r a
  -- *x // dereferenced or unquoted name
  unqu :: (Ord a) => r (RhoName a) -> r a
  -- @P // name or quoted process
  quo :: (Ord a) => r a -> r (RhoName a)

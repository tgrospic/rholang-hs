{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Rholang4b.Rholang where

import qualified Data.MultiSet as MS

-- RHO-calculus :: Deep embedding (collection)

data Process where
  Stop   :: Process
  Input  :: Process -> Process -> Process -> Process
  Output :: Process -> Process -> Process
  -- Par
  Par    :: MS.MultiSet Process -> Process
  -- Process bound/variable/ground
  Bound  :: Integer -> Process
  Var    :: String  -> Process
  Gnd    :: Ground  -> Process

deriving instance Eq Process
deriving instance Ord Process

-- Built-in types
data Ground
  = GBool   Bool
  | GInt    Integer
  | GString String
  | GUri    String
  | GUnforg Integer
  deriving (Eq, Ord)

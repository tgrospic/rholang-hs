{-# LANGUAGE TypeFamilies, RecordWildCards #-}

module Rholang2.Semantics where

import qualified Data.MultiSet as MS
import Rholang2.RhoFinal
import Rholang2.RhoInitial

import Debug.Trace
import Rholang2.Print

-- Rholang semantics

type instance TProcess Name = Process

type instance TName Process = Name

instance ProcessSymantics Process where
  nil = procEmpty
  -- substitute x for y in P `for( y <- x ) P`
  -- TODO: y cannot have holes e.g. `for( @{a | b} <- x )`
  for y@(VarName _) x p = procEmpty {
    input  = Just (True, x, substitute y (BoundName $ height p) p),
    height = height p + 1
  }
  for y x p = procEmpty {
    input  = Just (False, x, substitute y (BoundProc $ height p) p),
    height = height p + 1
  }
  out n p = procEmpty {
    output = Just (n, p),
    height = height p
  }
  p .| q = procEmpty {
    par    = MS.fromList [p, q],
    height = max (height p) (height q)
  }
  -- syntax erasure *@P = P
  eval (Name p) = p
  eval n        = procEmpty { reify = Just n }

-- substitute x for y in p
substitute :: Name -> Name -> Process -> Process
substitute y x = sub
  where
    sub Process{..} = procEmpty {
      input  = i input,
      output = o output,
      reify  = d reify,
      par    = MS.map (substitute y x) par,
      height = height
    }
    -- input
    i (Just (b, n, q)) = Just (b, if n == y then x else n, substitute y x q)
    i Nothing          = Nothing
    -- output
    o (Just (n, q)) = Just (if n == y then x else n, substitute y x q)
    o Nothing       = Nothing
    -- drop
    d (Just n) = Just $ if n == nn y then x else n
    d Nothing  = Nothing
    -- process variable is encoded as reified VarProc `*VarProc`
    -- @*VarProc == VarProc
    nn (Name p@Process{reify = Just n@(VarProc _)}) = n
    nn n                                            = n

instance NameSymantics Name where
  quo = Name

-- Substitute free variable with Unforgeable name
-- this is a proof we don't need to store `height`
freeNames :: Integer -> Process -> Process
freeNames i pr@Process{..} = res
  where
    res = procEmpty {
      input  = freeInput input,
      output = freeOutput output,
      reify  = free i <$> reify,
      par    = MS.map (freeNames i) par,
      height = height
    }
    -- Substitute input
    freeInput (Just (b, n, p)) = Just (b, free (i+1) n, freeNames i p)
    freeInput Nothing = Nothing
    -- Substitute output
    freeOutput (Just (n, p)) = Just (free i n, freeNames i p)
    freeOutput Nothing = Nothing
    -- replace free variable with Unforgeable name
    free :: Integer -> Name -> Name
    free h n@(VarProc x) = GndP (GUnforg h)
    free h n@(VarName x) = GndN (GUnforg h)
    free h n@(Name p)    = quo $ freeNames h p
    free _ n = n

{-# LANGUAGE TypeFamilies #-}

module Rholang3.Semantics where

import qualified Data.MultiSet as MS
import qualified Data.Map.Strict as M
import Rholang3.RhoFinal
import Rholang3.RhoInitial

import Debug.Trace
import Rholang3.Print

-- Rholang semantics

type instance TProcess Name = Process

type instance TName Process = Name

instance ProcessSymantics Process where
  nil = Stop
  for = Input
  out = Output
  p .| q = Par $ MS.fromList [p, q]
  -- Syntax erasure *@P = P
  eval (Name p) = p
  eval n        = Eval n

instance NameSymantics Name where
  -- Syntax erasure @*P == P
  quo (Eval n) = n
  quo p        = Name p

-- Substitute x for y in P `for( y <- x ) P`
substitute :: Name -> Name -> Process -> Process
substitute y x Stop            = Stop
substitute y x (Input y' x' p) = Input y' (if x' == y then x else x') (substitute y x p)
substitute y x (Output x' p)   = Output (if x' == y then x else x') (substitute y x p)
substitute y x (Eval x')       = Eval $ if x' == y then x else x'
substitute y x (Par ms)        = Par $ MS.map (substitute y x) ms

-- Substitute bound names
boundNames :: Integer -> Process -> Process
boundNames i Stop          = Stop
boundNames i (Input y x p) = Input y' x (boundNames (i+1) p') where
  -- TODO: y pattern cannot have holes e.g. `for( @{a!(b)} <- ... )`
  y' = bound i y
  p' = substitute y y' p
boundNames i (Par ms) = Par $ MS.map (boundNames i) ms
boundNames i x = x

bound :: Integer -> Name -> Name
bound h n@(Var t x) = Bound t h
bound _ n = n

-- Substitute free variable with Unforgeable name
freeNames :: Integer -> Process -> Process
freeNames i Stop          = Stop
freeNames i (Input y x p) = Input y x' p' where
  x' = free i x
  p' = freeNames (i+1) p
freeNames i (Output x p)  = Output (free i x) (freeNames i p)
freeNames i (Eval x)      = Eval (free i x)
freeNames i (Par ms)      = Par $ MS.map (freeNames i) ms

free :: Integer -> Name -> Name
free h n@(Var t x) = Gnd t (GUnforg h)
free h n@(Name p)  = quo $ freeNames h p
free _ n = n

new :: Foldable t => t String -> Process -> Process
new xs p = uncurry boundNames $ foldl sub (0, p) xs where
  sub (i, p') x = (i+1, substitute (Var N x) (Bound N i) p')

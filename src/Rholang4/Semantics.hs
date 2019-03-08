{-# LANGUAGE GADTs #-}

module Rholang4.Semantics where

import qualified Data.MultiSet as MS
import qualified Data.Map.Strict as M
import Rholang4.Rholang

import Debug.Trace
import Rholang4.Print

-- Rholang semantics

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
free h n@(Quote p) = Quote $ freeNames h p
free _ n = n

new :: Foldable t => t String -> Process -> Process
new xs p = uncurry boundNames $ foldl sub (0, p) xs where
  sub (i, p') x = (i+1, substitute (Var N x) (Bound N i) p')

{-# LANGUAGE GADTs #-}

module Rholang4b.Semantics where

import qualified Data.MultiSet as MS
import qualified Data.Map.Strict as M
import Rholang4b.Rholang

import Debug.Trace
import Rholang4b.Print

-- Rholang semantics

-- Substitute x for y in P `for( y <- x ) P`
substitute :: Process -> Process -> Process -> Process
substitute y x Stop            = Stop
substitute y x (Input y' x' p) = Input y' (if x'' == y then x else x'') (substitute y x p) where
  x'' = substitute y x x'
substitute y x (Output x' p)   = Output (if x'' == y then x else x'') (substitute y x p) where
  x'' = substitute y x x'
substitute y x (Par ms)        = Par $ MS.map (substitute y x) ms
substitute y x x'              = if x' == y then x else x'

-- Substitute bound variables
boundVars :: Integer -> Process -> Process
boundVars i Stop          = Stop
boundVars i (Input y x p) = Input y' x (boundVars (i+1) p') where
  -- TODO: y pattern cannot have holes e.g. `for( {a!(b)} <- ... )`
  y' = bound i y
  p' = substitute y y' p
boundVars i (Par ms) = Par $ MS.map (boundVars i) ms
boundVars i x = x

bound :: Integer -> Process -> Process
bound h n@(Var x) = Bound h
bound _ n = n

-- Substitute free variable with Unforgeable process
freeVars :: Integer -> Process -> Process
freeVars i Stop          = Stop
freeVars i (Input y x p) = Input y x' p' where
  x' = free i x
  p' = freeVars (i+1) p
freeVars i (Output x p)  = Output (free i x) (freeVars i p)
freeVars i (Par ms)      = Par $ MS.map (freeVars i) ms

free :: Integer -> Process -> Process
free h n@(Var x) = Gnd (GUnforg h)
free _ n = n

new :: Foldable t => t String -> Process -> Process
new xs p = uncurry boundVars $ foldl sub (0, p) xs where
  sub (i, p') x = (i+1, substitute (Var x) (Bound i) p')

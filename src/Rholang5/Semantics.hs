{-# LANGUAGE TypeFamilies #-}

module Rholang5.Semantics where

import qualified Data.MultiSet as MS
import qualified Data.Map.Strict as M
import Rholang5.RhoFinal
import Rholang5.RhoInitial

import Debug.Trace

-- Rholang semantics

newtype Rho a = Rho { unRho :: RhoT a }

type family RhoT a
type instance RhoT P = Process
type instance RhoT N = Name

instance ProcessSymantics Rho where
  nil       = Rho $ Par MS.empty
  par p q   = Rho $ Par $ MS.fromList [unRho p, unRho q]
  for x y p = Rho $ Input (unRho x) (unRho y) (unRho p)
  out x p   = Rho $ Output (unRho x) (unRho p)
  -- Syntax erasure *@P = P
  eval (Rho (Name p)) = Rho p
  eval n              = Rho . Eval . unRho $ n

instance NameSymantics Rho where
  -- Syntax erasure @*P == P
  quo (Rho (Eval n)) = Rho n
  quo p              = Rho . Name . unRho $ p

instance VariableSymantics Rho where
  nVar v = Rho $ Var N v
  pVar v = Rho $ Eval $ Var P v

instance GroundNameSymantics Rho where
  gInt x  = Rho $ Gnd N $ GInt x
  gStr x  = Rho $ Gnd N $ GString x
  gBool x = Rho $ Gnd N $ GBool x
  gUri x  = Rho $ Gnd N $ GUri x

instance RholangSymantics Rho

-- Substitute x for y in P `for( y <- x ) P`
substitute :: Name -> Name -> Process -> Process
substitute y x (Input y' x' p) = Input y' (if x' == y then x else x') (substitute y x p)
substitute y x (Output x' p)   = Output (if x' == y then x else x') (substitute y x p)
substitute y x (Eval x')       = Eval $ if x' == y then x else x'
substitute y x (Par ms)        = Par $ MS.map (substitute y x) ms

-- Substitute bound names
boundNames :: Integer -> Process -> Process
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
freeNames i (Input y x p) = Input y x' p' where
  x' = free i x
  p' = freeNames (i+1) p
freeNames i (Output x p)  = Output (free i x) (freeNames i p)
freeNames i (Eval x)      = Eval (free i x)
freeNames i (Par ms)      = Par $ MS.map (freeNames i) ms

free :: Integer -> Name -> Name
free h n@(Var t x) = Gnd t (GUnforg h)
free h n@(Name p)  = Name $ freeNames h p
free _ n = n

new :: Foldable t => t String -> Process -> Process
new xs p = uncurry boundNames $ foldl sub (0, p) xs where
  sub (i, p') x = (i+1, substitute (Var N x) (Bound N i) p')

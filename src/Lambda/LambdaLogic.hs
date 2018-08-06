{-# LANGUAGE TypeFamilies #-}

module Lambda.LambdaLogic where

import Data.Set

type family TName p

type family TLambda n

class LambdaSymantics exp where
  nop  :: exp
  lam  :: TName exp -> exp -> exp
  app  ::       exp -> exp -> exp

class NameSymantics n where
  var :: TLambda n -> n

data Lambda = Lam Var Lambda
            | App Lambda Lambda
            | Nop
            deriving (Eq, Ord, Show)

newtype Var = Var Lambda deriving (Eq, Ord, Show)

type instance TName Lambda = Var

type instance TLambda Var = Lambda

instance LambdaSymantics Lambda where
  lam  = Lam
  app  = App
  nop  = Nop

instance NameSymantics Var where
  var = Var

-- Logic for Lambda calculus

class LambdaLogicSymantics exp where
  true :: exp
  not' :: exp -> exp
  (&.) :: exp -> exp -> exp

data Logic = Exp (Set Logic) | Gi Lambda deriving (Eq, Ord, Show)

langTerms = fromList [Gi nop, Gi $ lam (var nop) nop, Gi $ app nop nop]

instance LambdaLogicSymantics Logic where
  true               = Exp langTerms
  (Exp a) &. (Exp b) = Exp $ a `intersection` b
  (Exp a) &. (Gi b)  = Exp $ a `intersection` singleton (Gi b)
  (Gi a)  &. (Exp b) = Exp $ singleton (Gi a) `intersection` b
  (Gi a)  &. (Gi b)  = Gi $ app a b
  not' (Exp a)       = Exp $ fold delete langTerms a
  not' a             = Exp $ a `delete` langTerms

false = not' true :: Logic

a |. b = not' (not' a &. not' b)

xor a b = not' (a &. b) &. (a |. b)

-- logic test example

truthTable = [
    true /= false,
    not' true == false,
    not' (not' false) == false,

    false &. false == false,
    true &. false  == false,
    false &. true  == false,
    true &. true   == (true :: Logic),

    false |. false == false,
    true |. false  == true,
    false |. true  == true,
    true |. true   == (true :: Logic),

    xor false false == false,
    xor true false  == true,
    xor false true  == true,
    xor true true   == false,

    xor (true &. (false |. xor true (xor false true))) ((false |. true) &. true ) == true
  ]

result = all (==True) truthTable

{-# LANGUAGE TypeFamilies #-}

module Lambda.Lambda where

type family TName p

type family TLambda n

class LambdaSymantics exp where
  nop  :: exp
  lam  :: TName exp -> exp -> exp
  app  ::       exp -> exp -> exp
  eval :: TName exp -> exp

class NameSymantics n where
  var :: TLambda n -> n

data Lambda = Lam Var Lambda
            | App Lambda Lambda
            | Eval Var
            | Nop
            deriving (Eq, Show)

data Var = Var Lambda
         | Address Int
         deriving (Eq, Show)

type instance TName Lambda = Var

type instance TLambda Var = Lambda

instance LambdaSymantics Lambda where
  lam  = Lam
  app  = App
  eval = Eval
  nop  = Nop

instance NameSymantics Var where
  var = Var

substitute :: Var -> Var -> Lambda -> Lambda
substitute y x (Lam a exp) = Lam a' (substitute y x exp)
  where a' = if a == x then y else a
substitute y x (App a b) = App a' b'
  where a' = substitute y x a
        b' = substitute y x b
substitute y x Nop = Nop
substitute y x (Eval a) = Eval a'
  where a' = if a == x then y else a

deBruijnify :: Lambda -> Int -> Lambda
deBruijnify Nop lvl = Nop
deBruijnify (Lam y e) lvl = Lam dbny e''
  where e''  = substitute dbny y e'
        e'   = deBruijnify e (lvl+1)
        dbny = Address lvl
deBruijnify (App e1 e2) lvl = App e1' e2'
  where e1' = deBruijnify e1 lvl
        e2' = deBruijnify e2 lvl
deBruijnify (Eval (Var px)) lvl = Eval x
  where x  = Var $ deBruijnify px lvl
deBruijnify (Eval (Address addr)) lvl = Eval (Address addr)



-- substitution example

n0 = var nop :: Var
n2 = var $ eval $ var $ eval $ n0
n3 = var $ eval n2

e2 = lam n0 $ lam n2 (eval n3) :: Lambda

e3 = lam n2 $ lam n0 (eval n2) :: Lambda

{-

> deBruijnify e2 0
Lam (Address 0) (Lam (Address 1) (Eval (Var (Eval (Var (Eval (Var (Eval (Var Nop)))))))))

> deBruijnify e3 0
Lam (Address 0) (Lam (Address 1) (Eval (Address 0)))

-}

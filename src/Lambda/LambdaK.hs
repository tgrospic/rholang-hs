{-# LANGUAGE TypeFamilies #-}

module Lambda.LambdaK where

type family TName p

type family TLambda n

type family TContext n

class LambdaSymantics exp where
  nop  :: exp
  lam  :: TName exp -> exp -> exp
  app  ::       exp -> exp -> exp
  eval :: TName exp -> exp
  -- location update
  update :: TName exp -> exp
  -- situation catalyst
  comm   :: TContext exp -> exp

class NameSymantics n where
  var :: TContext n -> TLambda n -> n

class ContextSymantics k where
  -- []
  hole :: k
  -- \x.M
  lamK :: TName k -> k -> k
  -- M N
  appK :: TLambda k -> k -> k

data Lambda = Lam Var Lambda
            | App Lambda Lambda
            | Eval Var
            | Nop
            | Update Var
            | Comm   Context
            deriving (Eq, Show)

data Var = Var Context Lambda
         | Address Int
         deriving (Eq, Show)

data Context
  = Hole
  | LamK Var    Context
  | AppK Lambda Context
  deriving (Eq, Show)

type instance TName Lambda  = Var
type instance TName Context = Var

type instance TLambda Var     = Lambda
type instance TLambda Context = Lambda

type instance TContext Var    = Context
type instance TContext Lambda = Context

instance LambdaSymantics Lambda where
  lam    = Lam
  app    = App
  eval   = Eval
  nop    = Nop
  update = Update
  comm   = Comm

instance NameSymantics Var where
  var = Var

instance ContextSymantics Context where
  hole = Hole
  lamK = LamK
  appK = AppK


substitute :: Var -> Var -> Lambda -> Lambda
substitute y x (Lam a exp) = Lam a' (substitute y x exp)
  where a' = if a == x then y else a
substitute y x (App a b) = App a' b'
  where a' = substitute y x a
        b' = substitute y x b
substitute y x Nop = Nop
substitute y x (Eval a) = Eval a'
  where a' = if a == x then y else a
substitute y x (Update a) = Update a'
  where a' = if a == x then y else a
substitute y x (Comm a) = Comm $ substituteK y x a

substituteK :: Var -> Var -> Context -> Context
substituteK y x Hole = Hole
substituteK y x (LamK a exp) = LamK a' (substituteK y x exp)
  where a' = if a == x then y else a
substituteK y x (AppK a b) = AppK a' b'
  where a' = substitute y x a
        b' = substituteK y x b

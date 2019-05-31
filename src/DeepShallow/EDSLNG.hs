{-# Language NoMonomorphismRestriction #-}

-- The modern reconstruction of
--    Extensible Denotational Language Specifications
--    Robert Cartwright, Matthias Felleisen
--    Theor. Aspects of Computer Software, 1994
-- and re-formulating their approach in terms of Extensible Effects.
-- The code for the talk `Having an Effect'


module DeepShallow.EDSLNG where

-- ========================================================================
-- Demonstrating the instability of denotations
-- in the conventional denotational semantics.

-- Universal domain

data Dom = DError |
           DInt Int | DBool Bool |
           DFun (Dom -> Dom)

instance Show Dom where
  show DError    = "<error>"
  show (DInt x)  = show x
  show (DBool x) = show x
  show (DFun _)  = "<fun>"

-- The first language to study
data Expr0 = E0Int Int | E0Inc | E0App Expr0 Expr0


-- Denotational semantics
-- (a more `practical' and less formal version, as an interpreter)
-- Denotational semantics is a compositional mapping from Expr to something else
-- or, a structurally-inductive interpreter -- a fold.
eval0 :: Expr0 -> Dom
eval0 (E0Int x) = DInt x
eval0 E0Inc     = DFun $ \x -> case x of
                          DInt n -> DInt (succ n)
                          _      -> DError
-- Compositionality: the meaning of application depends on the
-- meaning of e1 and e2, but not on e1 and e2 themselves (not on their
-- structure)
eval0 (E0App e1 e2) = case eval0 e1 of
  DFun f -> f (eval0 e2)
  _      -> DError

-- Useful injection for Int->Int functions
-- (In hand-written denotational semantics, such injections are usually
-- implicit)

injI :: (Int -> Int) -> Dom
injI f = DFun $ \x -> case x of
  DInt n -> DInt (f n)
  _      -> DError

tinc = E0App E0Inc (E0App E0Inc (E0Int 2))

_ = eval0 tinc


-- Extension: adding conditionals
-- We have to break the data type (it is not extensible)
data Expr1 = E1Int Int | E1Inc | E1App Expr1 Expr1
 -- new stuff
 | E1Eq
 | E1If Expr1 Expr1 Expr1

eval1 :: Expr1 -> Dom
eval1 (E1Int x) = DInt x
eval1 E1Inc     = injI succ
eval1 (E1App e1 e2) = case eval1 e1 of
  DFun f -> f (eval1 e2)
  _      -> DError
eval1 E1Eq = DFun $ \x -> case x of
  DInt n1 -> DFun $ \y -> case y of
    DInt n2 -> DBool (n1 == n2)
    _      -> DError
  _      -> DError
eval1 (E1If e et ef) = case eval1 e of
  DBool True  -> eval1 et
  DBool False -> eval1 ef
  _           -> DError

-- have to duplicate eval0

-- Cannot reuse tinc to use with eval1 or to use with new features
-- _ = eval1 tinc

-- Have to re-write it
tinc1 = E1App E1Inc (E1App E1Inc (E1Int 2))

tif = E1If (E1App (E1App E1Eq (E1Int 3)) tinc1)
       (E1Int 10) (E1App E1Inc tinc1)

_ = eval1 tif

-- Need extensible data types and extensible functions!
-- This isn't the problem of denot semantics, only our mechanization of it

-- Although it is somewhat telling. Usually, denot semantics is presented
-- mathematically, and mathematics is usually informal (although still
-- rigorous). Lots of problems do come up when we try to be formal.

-- Luckily, there is a way to make our interpreters extensible

class EBasic d where
  int :: Int -> d
  inc :: d
  app :: d -> d -> d

infixl 1 `app`

ttinc = inc `app` (inc `app` int 2)

-- Interpreters
-- There is no single function `eval'. Rather, the interpreters are
-- `wired-in'.
-- Lesson: a mathematical function doesn't have to correspond to
-- a function in code! Math (CT) is abstract; it's a blessing and a curse.

instance EBasic Dom where
  int = DInt
  inc = injI succ
  -- Compositionality is even more apparent
  app (DFun f) e2 = f e2
  app _ _         = DError

_ = ttinc :: Dom

-- Extension is now easy

class ECond d where
  eq  :: d
  if_ :: d -> d -> d -> d

instance ECond Dom where
  eq = DFun $ \x -> case x of
    DInt n1 -> DFun $ \y -> case y of
      DInt n2 -> DBool (n1 == n2)
      _      -> DError
    _      -> DError
  if_ (DBool True)  et ef = et
  if_ (DBool False) et ef = ef
  if_ _             _  _  = DError

-- No duplication of the EBasic interpreter
-- Can trivially evaluate ttinc in the `new' interpreter

-- Can reuse ttinc
ttif = if_ (eq `app` (int 3) `app` ttinc) (int 10) (inc `app` ttinc)

_ = ttif :: Dom


-- ------------------------------------------------------------------------
-- A substantial extension: state
-- Global Integer state

class GlobalIntState d where
  get :: d
  put :: d -> d


-- But which domain?
-- instance GlobalIntState Dom
-- will not work

newtype DomIntState = DIS (Int -> (Dom,Int))

instance GlobalIntState DomIntState where
  get = DIS (\s -> (DInt s,s))
  put (DIS e) = DIS $ \s ->
    case e s of
      (DInt s',_) -> (DInt s',s')
      (_,s)       -> (DError,s)

evalState :: DomIntState -> Int -> Dom
evalState (DIS e) s0 = fst $ e s0

-- We can reuse the old ttinc term
ttS = if_ (eq `app` (int 3) `app` (put ttinc))
       (put (int 10)) (put (inc `app` get))

-- See the type ttS

-- But we can't interpret it in Dom
-- We have to interpret in DomIntState
-- But for that, we have to re-write old interpreters!


instance EBasic DomIntState where
  int x = DIS $ \s -> (DInt x,s)
  inc   = DIS $ \s -> (injI succ,s)
  -- not-so-trivial re-writing
  -- and still isn't general enough
  -- QUIZ
  -- the function to apply isn't state-modifying
  app (DIS e1) (DIS e2) = DIS $ \s0 ->
    let (f,s1) = e1 s0
        (x,s2) = e2 s1
    in (app f x,s2)

instance ECond DomIntState where
  eq = DIS $ \s -> (eq,s)
  -- QUIZ Why couldn't we reuse the earlier if_?
  if_ (DIS e)  (DIS et) (DIS ef) = DIS $ \s -> case e s of
    (DBool True,s)  -> et s
    (DBool False,s) -> ef s

_ = evalState ttS 0

-- We had to re-write all earlier interpreters!
-- Especially if_, non-trivially
-- But the `ideal` meaning of inc has not changed with the addition of state
-- Why we had to re-write its denotation? The `true' meaning of
-- equality testing and integers hasn't changed either!

-- What if I want state with multiple variables?
-- rewrite all the interpreters yet again?
-- Most of which couldn't care about state!

-- Another extension: Higher-order functions

type VarName = String

class Lam d where
  var :: VarName -> d
  lam :: VarName -> d -> d

-- Can we use the same Dom or DomIntState?
-- How to distinguish CBN with CBV?
-- We need yet another domain

-- newtype DomIntState = DIS (Env -> Dom)
-- type Env = VarName -> Dom

-- This one? Would it really work?
-- (hint: multi-arg functions).

-- What if I want coroutines or non-determinism? yet another extension
-- of the domain!


-- ========================================================================
-- EDLS idea, but formulated as  Freer monad


data Comp = Done DomC
          | Req ReqT (DomC -> Comp)

-- No DError any more
data DomC = DCInt Int | DCBool Bool |
            DCFun (DomC -> Comp)

-- For now, request type is not extensible
data ReqT = ReqError | ReqState StateReq | ReqHO HOReq
                       deriving Show

err :: Comp
err = Req ReqError (\_ -> err)

instance EBasic Comp where
  int = Done . DCInt
  inc = Done . DCFun $ \x -> case x of
    DCInt x -> int (succ x)
    _       -> err
  app (Done (DCFun f)) (Done e2) = f e2
  app (Done _) (Done e2)         = err
  app (Req r k) e2 = Req r (\x -> app (k x) e2)
  app e1 (Req r k) = Req r (\x -> app e1 (k x))


instance Show Comp where
  show (Done x)  = show x
  show (Req r _) = show r

instance Show DomC where
  show (DCInt x)  = show x
  show (DCBool x) = show x
  show (DCFun _)  = "<fun>"

-- Evaluating the same term ttinc
_ = ttinc :: Comp


instance ECond Comp where
  eq = Done . DCFun $ \x -> case x of
    DCInt n1 -> Done . DCFun $ \y -> case y of
      DCInt n2 -> Done . DCBool $ (n1 == n2)
      _      -> err
    _      -> err
{-
  if_ (Done (DCBool True))  et ef = et
  if_ (Done (DCBool False)) et ef = ef
  if_ (Done _)              _  _  = err
  if_ (Req r k) et ef = Req r (\x -> if_ (k x) et ef)
-}
 -- we see the common pattern. Rewrite using bind
  if_ e et ef = bind e $ \x -> case x of
    DCBool True  -> et
    DCBool False -> ef
    _            -> err

bind :: Comp -> (DomC -> Comp) -> Comp
bind (Done x) k    = k x
bind (Req r k1) k2 = Req r (\x -> bind (k1 x) k2)

-- Monadic Structure! Moggi!

_ = ttif :: Comp

-- So far, the only request is error propagation

data StateReq = Get | Put Int
                      deriving Show


instance GlobalIntState Comp where
  get   = Req (ReqState Get) Done
  put e = bind e $ \x -> case x of
    DCInt x -> Req (ReqState (Put x)) Done
    _       -> err

_ = ttS :: Comp

-- Unhandled request

-- The handler of ReqState, the authority that administers the State
-- resource.
-- The handler is recursive; so to obtain its meaning we should take
-- a fix-point.

handleState :: Int -> Comp -> Comp
handleState _ (Done x)    = Done x
handleState s (Req (ReqState r) k) = case r of
  Get   -> handleState s $ k (DCInt s)
  Put s -> handleState s $ k (DCInt s)
-- everything else
handleState s (Req r k) = Req r (handleState s . k)

_ = handleState 0 ttS
-- 5

-- No longer any duplication of the previous interpreters.
-- They are written once and forall

-- ------------------------------------------------------------------------
-- Higher-order functions

data HOReq = ReqVar VarName | ReqClosure VarName Comp
                              deriving Show

{-
instance Lam Comp where
  var v      = Req (ReqHO (ReqVar v)) Done
  lam v body = Done . DCFun $ \x -> handleVarD [(v,x)] body
-}

type Env = [(VarName,DomC)]

handleVarD :: Env -> Comp -> Comp
handleVarD _ (Done x) = Done x
handleVarD env (Req (ReqHO (ReqVar v)) k) | Just x <- lookup v env =
  handleVarD env $ k x
-- everything else
handleVarD env (Req r k) = Req r (handleVarD env . k)

th0 = lam "x" (inc `app` var "x")

_ = th0 :: Comp

_ = th0 `app` ttinc :: Comp

-- Is this OK?

th1 = lam "x" $ lam "y" $
        if_ (eq `app` var "x" `app` var "y") (var "x") (var "y")

th2 = (lam "z" $ lam "x" $ var "z" `app` var "x") `app` (th1 `app` int 1)
  `app` (int 2)

_ = th2 :: Comp

-- What went wrong?



instance Lam Comp where
  var v      = Req (ReqHO (ReqVar v)) Done
  lam v body = Req (ReqHO (ReqClosure v body)) Done

handleVar :: Env -> Comp -> Comp
handleVar _ (Done x) = Done x
handleVar env (Req (ReqHO r) k) = case r of
  ReqVar v | Just x <- lookup v env -> handleVar env $ k x
  ReqVar _  -> err
  ReqClosure v body ->
    handleVar env $ k (DCFun $ \x -> handleVar ((v,x):env) body)
-- everything else
handleVar env (Req r k) = Req r (handleVar env . k)

_ = th0 :: Comp

_ = handleVar [] th0 :: Comp

_ = handleVar [] (th0 `app` ttinc) :: Comp

-- Is this OK?

_ = handleVar [] th2 :: Comp
-- 2

-- So, closure creation is an effect!
-- (compare with ATS).

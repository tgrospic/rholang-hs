{-# LANGUAGE TypeFamilies #-}

-- This implementation follows original version by L.G.Meredith
-- https://github.com/AkbarsGrasp/RhoMachine/blob/9ac6b0af08d6272638314eb9d850392996f2ef8f/RhoCalc.lhs

module Rho.Rho where

class ProcessSymantics p where
  type TName p
  -- 0 // nil or stopped process
  nil  :: p
  -- for( y <- x ) P // input guarded process
  for  :: TName p -> TName p -> p -> p
  -- x!( @Q ) // output
  out  :: TName p -> p -> p
  -- P|Q // parallel composition
  (.|) :: p -> p -> p
  -- *x // dereferenced or unquoted name
  eval :: TName p -> p

class NameSymantics n where
  type TProcess n
  -- @P // name or quoted process
  quo :: TProcess n -> n

data Process a
  = Stop
  | Input  (Name a)    (Name a)    (Process a)
  | Output (Name a)    (Process a)
  | Par    (Process a) (Process a)
  | Eval   (Name a)
  deriving (Eq, Show)

data Name a = Name (Process a)
            | Address Int
            deriving (Eq, Show)

instance ProcessSymantics (Process a) where
  type TName (Process a) = Name a
  nil  = Stop
  for  = Input
  out  = Output
  (.|) = Par
  eval = Eval

instance NameSymantics (Name a) where
  type TProcess (Name a) = Process a
  quo  = Name

-- https://github.com/AkbarsGrasp/RhoMachine/blob/9ac6b0af08d6272638314eb9d850392996f2ef8f/RhoCalc.lhs#L119
substitute :: Name a -> Name a -> Process a -> Process a
substitute y x Stop = Stop
substitute y x (Input a b q) = Input a' b' q'
  where a'  = if a == x then y else a
        b'  = if b == x then Name (Par (Eval b) q) else b
        q'  = substitute y x q''
        q'' = if b == x then substitute b' b q else q
substitute y x (Output a q) = Output a' q'
  where a' = if a == x then y else a
        q' = substitute y x q
substitute y x (Par p q) = Par p' q'
  where p' = substitute y x p
        q' = substitute y x q
substitute y x (Eval a) = Eval a'
  where a' = if a == x then y else a

-- https://github.com/AkbarsGrasp/RhoMachine/blob/9ac6b0af08d6272638314eb9d850392996f2ef8f/RhoCalc.lhs#L135
deBruijnify :: Process a -> Int -> Int -> Int -> Process a
deBruijnify Stop l w h = Stop
deBruijnify (Input (Name px) y q) l w h = Input x dbny q''
  where q''    = substitute dbny y q'
        q'     = deBruijnify q (l+1) w h
        x      = Name (deBruijnify px l w (h+1))
        dbny   = Address dbnidx
        dbnidx = toNumber ((toBits l) ++ (toBits w) ++ (toBits h))
deBruijnify (Output (Name px) q) l w h = (Output x q')
  where x  = Name (deBruijnify px l w (h+1))
        q' = deBruijnify q l w h
deBruijnify (Par p q) l w h = Par p' q'
  where p' = deBruijnify p l w h
        q' = deBruijnify q l (w+1) h
deBruijnify (Eval (Name px)) l w h = Eval x
  where x  = Name (deBruijnify px l w (h+1))
deBruijnify (Eval (Address addr)) l w h = Eval (Address addr)

toNumber :: [Int] -> Int
toNumber [] = 0
toNumber l@(x:xs) = 2^((length l) - 1) * x + (toNumber xs)
--x - ((logBase 2 x)  | listlength = ((logBase 2 x) + 1) --subtract 1 from this every recursion
--this is your first value in the list
toBits :: Int -> [Int]
toBits 0 = []
toBits x = [1] ++ l
  where l = (take (m - n) (repeat 0)) ++ (if ((fromIntegral m) == d) then [] else r)
        m = (floor (realToFrac d))
        d = (logBase (fromIntegral 2) (fromIntegral x))
        n = (if ((fromIntegral m) == d) then 0 else (length r))
        r = (toBits (x - m))



-- substitution example

n1 = quo nil :: Name ()
n3 = quo (eval (quo (eval(n1))))

p1 = eval n3 .| nil :: Process ()

p2 = for n1 n3 p1

p3 = for n3 n1 p1

{-

> deBruijnify p2 0 0 0
Input (Name Stop) (Address 0) (Par (Eval (Address 0)) Stop)

> deBruijnify p3 0 0 0
Input (Name (Eval (Name (Eval (Name Stop))))) (Address 0) (Par (Eval (Name (Eval (Name (Eval (Name Stop)))))) Stop)

-}

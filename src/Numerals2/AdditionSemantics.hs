{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, MultiParamTypeClasses #-}

module Numerals2.AdditionSemantics where

import Data.List (intercalate)
import qualified Data.MultiSet as MS
import Numerals2.Numerals
import Numerals2.ArabicSemantics
import Numerals2.RomanSemantics

data Addition a where
  Add :: MS.MultiSet (Addition a) -> Addition a
  Gen :: a -> Addition a
  deriving (Eq, Ord)

type AdditionPair = Addition (Int, String)

-- Arabic numerals

instance ArabicAddSymantics (Addition Int) where
  type ArabicGen (Addition Int) = AN
  ar A0 = mempty
  ar a  = Gen $ evalAN a

instance ArabicAddSymantics (Addition String) where
  type ArabicGen (Addition String) = AN
  ar A0 = mempty
  ar a  = Gen $ showAN a

instance ArabicAddSymantics AdditionPair where
  type ArabicGen AdditionPair = AN
  ar A0 = mempty
  ar a  = Gen (evalAN a, showAN a)

-- Roman numerals

instance RomanAddSymantics (Addition Int) where
  type RomanGen (Addition Int) = RN
  ro a  = Gen $ evalRN a

instance RomanAddSymantics (Addition String) where
  type RomanGen (Addition String) = RN
  ro a  = Gen $ showRN a

instance RomanAddSymantics AdditionPair where
  type RomanGen AdditionPair = RN
  ro a = Gen (evalRN a, showRN a)

instance Ord a => Monoid (Addition a) where
  -- Zero is just an empty addition
  mempty = Add MS.empty

instance Ord a => Semigroup (Addition a) where
  -- Structural equivalance
  -- MultiSet structure gives commutativity: 1 + 2 ≡ 2 + 1
  -- and forgetting history gives associativity: (1 + 2) + 3 ≡ 1 + (2 + 3)
  Add a <> Add b = Add $ a `MS.union` b
  Add a <> b     = Add $ b `MS.insert` a
  a     <> Add b = Add $ a `MS.insert` b
  a     <> b     = Add $ MS.fromList [a, b]

instance Ord a => AdditionSymantics (Addition a)

eval :: ([a] -> a) -> (b -> a) -> Addition b -> a
eval g f (Add xs) = g $ eval g f <$> MS.toList xs
eval _ f (Gen a)  = f a

evalSum :: (a -> Int) -> Addition a -> Int
evalSum = eval sum

-- Equality

isStructEq :: Eq a => Addition a -> Addition a -> Bool
isStructEq a b = a == b

isEq1 :: AdditionPair -> AdditionPair -> Bool
isEq1 a b = evalSum fst a == evalSum fst b

-- Print

instance Show AdditionPair where
  show a = "(" <> vals a <> ", " <> text a <> ")" where
    vals = eval joinPlus (show . fst)
    text = eval joinPlus snd

instance Show (Addition String) where
  show = eval joinPlus id

instance Show (Addition Int) where
  show = eval joinPlus show

joinPlus = intercalate " + "

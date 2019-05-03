{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, StandaloneDeriving #-}

module Numerals3.AdditionSemantics where

import Data.List (intercalate)
import qualified Data.MultiSet as MS
import Numerals3.Numerals
import Numerals3.ArabicSemantics
import Numerals3.RomanSemantics

type Addition a = Calc a MS.MultiSet

type AdditionPair = Addition (Int, String)

deriving instance (Eq a)  => Eq  (Calc a MS.MultiSet)
deriving instance (Ord a) => Ord (Calc a MS.MultiSet)

-- Arabic numerals

instance ArabicAddSymantics (Addition Int) where
  type ArabicGen (Addition Int) = AN
  ar (Rec []) = mempty
  ar a        = Gen $ evalAN a

instance ArabicAddSymantics (Addition String) where
  type ArabicGen (Addition String) = AN
  ar (Rec []) = mempty
  ar a        = Gen $ showAN a

instance ArabicAddSymantics AdditionPair where
  type ArabicGen AdditionPair = AN
  ar (Rec []) = mempty
  ar a        = Gen (evalAN a, showAN a)

-- Roman numerals

instance RomanAddSymantics (Addition Int) where
  type RomanGen (Addition Int) = RN
  ro a = Gen $ evalRN a

instance RomanAddSymantics (Addition String) where
  type RomanGen (Addition String) = RN
  ro a = Gen $ showRN a

instance RomanAddSymantics AdditionPair where
  type RomanGen AdditionPair = RN
  ro a = Gen (evalRN a, showRN a)

instance Ord a => Monoid (Addition a) where
  -- Zero is just an empty addition
  mempty = Rec MS.empty

instance Ord a => Semigroup (Addition a) where
  -- Structural equivalance
  -- MultiSet structure gives commutativity: 1 + 2 ≡ 2 + 1
  -- and forgetting history gives associativity: (1 + 2) + 3 ≡ 1 + (2 + 3)
  Rec a <> Rec b = Rec $ a `MS.union` b
  Rec a <> b     = Rec $ b `MS.insert` a
  a     <> Rec b = Rec $ a `MS.insert` b
  a     <> b     = Rec $ MS.fromList [a, b]

instance Ord a => AdditionSymantics (Addition a)

eval :: ([a] -> a) -> (b -> a) -> Addition b -> a
eval g f (Rec xs) = g $ eval g f <$> MS.toList xs
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

{-# LANGUAGE NoMonomorphismRestriction #-}

module RhoImpl where

import Rholang
import Data.Set
import Prelude hiding (map)

samplePrint :: IO ()
samplePrint = printRho sampleProc2
           *> putStrLn (names sampleProc2)
           *> putStrLn ""
           *> putStr (serialize sampleProc3)
           *> putStrLn (names sampleProc3)

-- Sample Rho terms

sampleProc = nil .| for "c" "y" (out "c" nil)

sampleProc2 = for "a" "z" nil .| out "a" (quo sampleProc)

sampleProc3 = for "a" "z" (for "y" "a" sampleProc)

-- Type error: Couldn't match type `String' with `RhoName String'
-- sampleError = for "a" "z" nil .| out "a" sampleProc


-- Rho printer

newtype Printer a = Printer (IO ())

instance RhoProc Printer where
  nil        = Printer $ putStr "0"
  for a b p  = Printer $ putStr ("for(" ++ show a ++ " <- " ++ show b ++ ")") *> putStr "{ " *> printRho p *> putStr " }"
  out a name = Printer $ putStr (show a) *> putStr "!" *> printRho name
  a .| b     = Printer $ printRho a *> putStr " | " *> printRho b
  unqu a     = Printer $ putStr "*" *> printRho a
  quo (Printer proc) = Printer $ putStr "@" *> proc

printRho (Printer p) = p


-- Rho serializer

newtype RhoSerialize a = RhoSerialize String

instance RhoProc RhoSerialize where
  nil        = RhoSerialize "0"
  for a b p  = RhoSerialize $ "for(" ++ show a ++ " <- " ++ show b ++ ")" ++ "{ " ++ serialize p ++ " }"
  out a name = RhoSerialize $ show a ++ "!" ++ serialize name
  a .| b     = RhoSerialize $ serialize a ++ " | " ++ serialize b
  unqu a     = RhoSerialize $ "*" ++ serialize a
  quo (RhoSerialize proc) = RhoSerialize $ "@" ++ proc

serialize (RhoSerialize p) = p


-- Free names

instance RhoProc Set where
  nil       = empty
  for a b p = b `insert` (a `delete` p)
  out a p   = a `insert` (unqu p)
  a .| b    = a `union` b
  unqu a    = (\(RhoName x) -> x) `map` a
  quo p     = RhoName `map` p

names p = "\nNames:\n" ++ (show $ toList p)

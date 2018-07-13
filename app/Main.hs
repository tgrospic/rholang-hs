module Main where

import System.Environment
import RhoImpl
import Data.List
import Numerals.ArabicNumeralsImpl
import Numerals.ArabicNumeralsRepl
import Control.Monad

main :: IO ()
main = do
  args <- getArgs

  putStrLn $ (++"\n") =<< testNumeralsEq

  -- Start repl
  case find (=="--repl") args of
    Just _  -> repl
    Nothing -> pure ()

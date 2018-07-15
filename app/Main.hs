module Main where

import System.Environment
import Control.Monad
import Data.List
import RhoImpl
import Numerals.ArabicNumeralsImpl
import Numerals.ArabicNumeralsRepl

main :: IO ()
main = do
  args <- getArgs

  putStrLn $ testNumeralsEq >>= (++"\n")

  -- Start repl
  case find (=="--repl") args of
    Just _  -> repl
    Nothing -> pure ()

module Main where

import System.Environment
import Control.Monad
import Data.List
import System.Console.Haskeline
import qualified Numerals2.NumeralsRepl as NumRepl
import qualified RholangRepl as RhoRepl

main :: IO ()
main = do
  args <- getArgs

  -- putStrLn $ testNumeralsEq >>= (++"\n")

  -- Start repl
  let settings = defaultSettings { autoAddHistory=False }
  case find (=="repl") args of
    Just _  -> runInputT settings RhoRepl.repl
    Nothing -> pure ()

  case find (=="repl-num") args of
    Just _  -> runInputT settings NumRepl.repl
    Nothing -> pure ()

module Numerals.ArabicNumeralsRepl
  ( repl
  ) where

import System.Console.Readline
import Numerals.ArabicNumeralsImpl
import Numerals.ArabicNumeralsParser

repl :: IO ()
repl = do
  maybeLine <- readline "% "
  case maybeLine of
    Nothing     -> pure () -- EOF / control-d
    Just "exit" -> pure ()
    Just line -> do
      addHistory line
      case run arabicParser line of
        Left error -> putStrLn $ "Parse ERROR: " ++ error
        Right exp  -> do
          putStrLn $ "Parsed exp.:    " ++ show4 exp
          putStrLn $ "Evaluated exp.: " ++ show (eval4 exp)
      repl

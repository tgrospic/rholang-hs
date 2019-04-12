module Numerals1.ArabicNumeralsRepl
  ( repl
  ) where

import System.Console.Haskeline
import System.Console.Haskeline.History
import Numerals1.ArabicNumeralsImpl
import Numerals1.ArabicNumeralsParser

repl :: InputT IO ()
repl = do
  maybeLine <- getInputLine "% "
  case maybeLine of
    Nothing     -> pure () -- EOF / control-d
    Just "exit" -> pure ()
    Just line -> do
      h <- getHistory
      putHistory $ addHistoryUnlessConsecutiveDupe line h
      case run arabicParser line of
        Left error -> outputStrLn $ "Parse ERROR: " ++ error
        Right exp  -> do
          outputStrLn $ "Parsed exp.:    " ++ show4 exp
          outputStrLn $ "Evaluated exp.: " ++ show (eval4 exp)
      repl

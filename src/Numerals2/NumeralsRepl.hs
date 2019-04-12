module Numerals2.NumeralsRepl (repl) where

import System.Console.Haskeline
import System.Console.Haskeline.History
import Numerals2.AdditionSemantics
import Numerals2.AdditionSyntax

repl :: InputT IO ()
repl = do
  maybeLine <- getInputLine "% "
  case maybeLine of
    Nothing     -> pure () -- EOF / control-d
    Just "exit" -> pure ()
    Just line -> do
      h <- getHistory
      putHistory $ addHistoryUnlessConsecutiveDupe line h
      case run pAddition line of
        Left error -> outputStrLn $ "Parse ERROR: " ++ show error
        Right exp  -> do
          outputStrLn $ "Parsed exp.:    " ++ show (exp :: AdditionPair)
          outputStrLn $ "Evaluated exp.: " ++ show (evalSum fst exp)
      repl

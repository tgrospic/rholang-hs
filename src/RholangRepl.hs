module RholangRepl
  ( repl
  ) where

import System.Console.Haskeline
import System.Console.Haskeline.History
import Rholang2.Syntax

repl :: InputT IO ()
repl = do
  maybeLine <- getInputLine "\nrholang $ "
  case maybeLine of
    Nothing     -> pure () -- EOF / control-d
    Just "exit" -> pure ()
    Just []     -> repl
    Just line -> do
      h <- getHistory
      putHistory $ addHistoryUnlessConsecutiveDupe line h
      case rhoParse pPar line of
        Left error -> outputStrLn $ show error
        Right exp  ->
          outputStrLn $ "Parsed:\n\n" ++ show exp <> "\n"
      repl

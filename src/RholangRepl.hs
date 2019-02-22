module RholangRepl
  ( repl
  ) where

import System.Console.Haskeline
import System.Console.Haskeline.History
import Rholang3.Syntax
import Rholang3.Semantics

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
          let parsed = new ["stdout", "stderr"] exp in
          outputStrLn $ "Parsed:\n\n" ++ show parsed <> "\n"
      repl

module RholangRepl
  ( repl
  ) where

import System.Console.Haskeline
import System.Console.Haskeline.History
import Rholang5.Syntax
import Rholang5.Semantics
import Rholang5.Print

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
      case rhoParse line of
        Left error -> outputStrLn $ show error
        Right exp  ->
          let parsed = new ["stdout", "stderr"] (unRho exp) in
          outputStrLn $ "Parsed:\n\n" ++ show parsed <> "\n"
      repl

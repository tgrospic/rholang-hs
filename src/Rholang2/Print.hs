{-# LANGUAGE RecordWildCards #-}

module Rholang2.Print where

import Control.Monad (join)
import Data.List (intercalate, replicate)
import qualified Data.MultiSet as MS
import Rholang2.RhoFinal
import Rholang2.RhoInitial
import Data.Either

instance Show Name where
  show (Name p)      = quoted $ show p
  show (BoundProc i) = 'x' : show i
  show (BoundName i) = 'x' : show i
  show (VarProc n)   = n
  show (VarName n)   = n
  show (GndP g)      = show g
  show (GndN g)      = show g

instance Show Ground where
  show (GBool x)   = show x
  show (GInt x)    = show x
  show (GString x) = show x
  show (GUri x)    = "Uri(" <> show x <> ")"
  show (GUnforg x) = "Unforg(" <> show x <> ")"

quoted p = "@{" <> p <> "}"

instance Show Process where
  show = showIndent 0
  -- show = showInline

-- Print inline

showInline pr@Process{..} = if pr == procEmpty then "Nil" else join procs
  where
    procs = [ showInput  input
            , showOutput output
            , showReify  reify
            , intercalate " | " $ showPar <$> MS.toOccurList par
            ]
    showPar (p, i) = join $ replicate i $ showInline p
    -- Print input
    -- Bound Process variable from pattern `for( ... <- @{x} )`
    showInput (Just (b, n@(BoundProc _), p)) = "for( " <> showBound b boundName <> " <- " <> quoted (show n) <> " ) { " <> show p <> " }"
    showInput (Just (b, n,               p)) = "for( " <> showBound b boundName <> " <- " <> show n <> " ) { " <> show p <> " }"
    showInput Nothing                        = ""
    showBound isName n = if isName then n else quoted n
    boundName = 'x' : show (height-1)
    -- Print output
    -- Bound Process variable from pattern `@{x}!(...)`
    showOutput (Just (n@(BoundProc _), p)) = quoted (show n) <> "!(" <> show p <> ")"
    showOutput (Just (n,               p)) = show n <> "!(" <> show p <> ")"
    showOutput Nothing                     = ""
    -- Print drop
    -- Bound Name variable from pattern `for( x <- ... ) { *x }`
    showReify (Just n@(BoundName _)) = '*' : show n
    showReify (Just n)               = show n
    showReify Nothing                = ""

-- Print with indentation

showIndent :: Int -> Process -> String
showIndent ind pr@Process{..} = if pr == procEmpty then "Nil" else join procs
  where
    procs = [ showInput  input
            , showOutput output
            , showReify  reify
            , intercalate " | " $ showPar <$> MS.toOccurList par
            ]
    showPar (p, i) = join $ replicate i $ showIndent ind p
    -- Print input
    -- Bound Process variable from pattern `for( ... <- @{x} )`
    showInput (Just (b, n@(BoundProc _), p)) = "for( " <> showBound b boundName <> " <- " <> quoted (show n) <> " ) {\n" <> indent (ind+1) <> showIndent (ind+1) p <> "\n" <> indent ind <> "}"
    showInput (Just (b, n,               p)) = "for( " <> showBound b boundName <> " <- " <> show n <> " ) {\n" <> indent (ind+1) <> showIndent (ind+1) p <> "\n" <> indent ind <>  "}"
    showInput Nothing                        = ""
    showBound isName n = if isName then n else quoted n
    boundName = 'x' : show (height-1)
    -- Print output
    -- Bound Process variable from pattern `@{x}!(...)`
    showOutput (Just (n@(BoundProc _), p)) = quoted (show n) <> "!(" <> show p <> ")"
    showOutput (Just (n,               p)) = show n <> "!(" <> show p <> ")"
    showOutput Nothing                     = ""
    -- Print drop
    -- Bound Name variable from pattern `for( x <- ... ) { *x }`
    showReify (Just n@(BoundName _)) = ('*' : show n)
    showReify (Just n)               = show n
    showReify Nothing                = ""
    -- Display
    indent x = join (replicate x "  ")

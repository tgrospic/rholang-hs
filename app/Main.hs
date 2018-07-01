module Main where

import RhoImpl
import Numerals.ArabicNumeralsImpl
import Control.Monad

main :: IO ()
main = putStrLn $ join $ (++"\n") <$> testNumeralsEq

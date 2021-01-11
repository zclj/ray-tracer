module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import TuplesSpec

main = defaultMain tupleTests
-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"

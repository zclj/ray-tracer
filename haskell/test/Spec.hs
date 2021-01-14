module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import TuplesSpec

-- integrate HSpec with Tasty
-- https://github.com/mitchellwrosen/tasty-hspec/issues/12
-- https://hackage.haskell.org/package/tasty-hspec-1.1.5.1/docs/Test-Tasty-Hspec.html#g:4

main = defaultMain tupleTests
-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"

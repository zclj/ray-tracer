module TuplesSpec where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Tuples as SUT

tupleTests :: TestTree
tupleTests = testGroup "Tuple Tests" [
  testGroup "Hunit tests" [ tuplePoint ]]

{-
Scenario: A tuple with w=1.0 is a point
  Given a ← tuple(4.3, -4.2, 3.1, 1.0)
  Then a.x = 4.3
    And a.y = -4.2
    And a.z = 3.1
    And a.w = 1.0
    And a is a point
    And a is not a vector

-}
tuplePoint = HU.testCase "A tuple with w=1.0 is a point" $
  let t = (SUT.Tuple 4.3 (-4.2) 3.1 1.0)
  in (do assertEqual "x" (x t) 4.3
         assertEqual "y" (y t) (-4.2)
         assertEqual "z" (z t) 3.1
         assertEqual "w" (w t) 1.0
         assertBool "is a point" (isPoint t)
         assertBool "is not a vector" (not (isVector t)))

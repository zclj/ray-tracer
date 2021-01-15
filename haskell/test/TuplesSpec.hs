module TuplesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS
import Tuples as SUT

{- After an initial evaluation, I find that Hspec provide a cleaner way of
   defining tests. Therefor Hspec will be used.

   - http://hspec.github.io/writing-specs.html

-}

tupleTests :: TestTree
tupleTests = testGroup "Tuple Tests" [
  testGroup "HSpec tests"
  [ unsafePerformIO (testSpec "Tuple" tupleBasics),
    unsafePerformIO (testSpec "Tuple" tupleArithmetic)]]

tupleBasics :: Spec
tupleBasics =
  describe "Basics" $ do
    {- Scenario: A tuple with w=1.0 is a point
         Given a ← tuple(4.3, -4.2, 3.1, 1.0)
         Then a.x = 4.3
           And a.y = -4.2
           And a.z = 3.1
           And a.w = 1.0
           And a is a point
           And a is not a vector -}
    describe "A tuple with w=1.0 is a point" $ do
      let a = (SUT.Tuple 4.3 (-4.2) 3.1 1.0)
      it "returns x" $ do
        x a `shouldBe` 4.3

      it "returns y" $ do
        y a `shouldBe` (-4.2)

      it "returns z" $ do
        z a `shouldBe` 3.1

      it "returns w" $ do
        w a `shouldBe` 1.0

      it "is a point" $ do
        a `shouldSatisfy` isPoint

      it "is not a vector" $ do
        a `shouldSatisfy` (not . isVector)

    {- Scenario: A tuple with w=0 is a vector
       Given a ← tuple(4.3, -4.2, 3.1, 0.0)
         Then a.x = 4.3
           And a.y = -4.2
           And a.z = 3.1
           And a.w = 0.0
           And a is not a point
           And a is a vector -}
    describe "A tuple with w=0 is a vector" $ do
      let a = (SUT.Tuple 4.3 (-4.2) 3.1 0.0)
      it "returns x" $ do
        x a `shouldBe` 4.3

      it "returns y" $ do
        y a `shouldBe` (-4.2)

      it "returns z" $ do
        z a `shouldBe` 3.1

      it "returns w" $ do
        w a `shouldBe` 0.0

      it "is not a point" $ do
        a `shouldSatisfy` (not . isPoint)

      it "is a vector" $ do
        a `shouldSatisfy` isVector

    {- Scenario: point() creates tuples with w=1
         Given p ← point(4, -4, 3)
         Then p = tuple(4, -4, 3, 1) -}
    describe "point creates tuples with w=1" $ do
      it "point equals tuple" $ do
        SUT.point 4 (-4) 3 `shouldBe` SUT.Tuple 4 (-4) 3 1

    {- Scenario: vector() creates tuples with w=0
         Given v ← vector(4, -4, 3)
         Then v = tuple(4, -4, 3, 0) -}
    describe "vector creates tuples with w=0" $ do
      it "vector equals tuple" $ do
        SUT.vector 4 (-4) 3 `shouldBe` SUT.Tuple 4 (-4) 3 0

tupleArithmetic :: Spec
tupleArithmetic =
  describe "Arithmetic" $ do
    {- Scenario: Adding two tuples
         Given a1 ← tuple(3, -2, 5, 1)
           And a2 ← tuple(-2, 3, 1, 0)
         Then a1 + a2 = tuple(1, 1, 6, 1) -}
    describe "Add" $ do
      it "adds two tuples" $ do
        pendingWith "Implementation"


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
        let a1 = (SUT.Tuple 3 (-2) 5 1)
            a2 = (SUT.Tuple (-2) 3 1 0)
        a1 `add` a2 `shouldBe` SUT.Tuple 1 1 6 1
          
    {- Scenario: Subtracting two points
         Given p1 ← point(3, 2, 1)
           And p2 ← point(5, 6, 7)
         Then p1 - p2 = vector(-2, -4, -6) -}
    describe "Sub" $ do
      it "subtracts two points" $ do
        let p1 = (SUT.point 3 2 1)
            p2 = (SUT.point 5 6 7)
        p1 `sub` p2 `shouldBe` SUT.vector (-2) (-4) (-6)

      {- Scenario: Subtracting a vector from a point
           Given p ← point(3, 2, 1)
             And v ← vector(5, 6, 7)
           Then p - v = point(-2, -4, -6) -}
      it "subtracts a vector from a point" $ do
        let p = (SUT.point 3 2 1)
            v = (SUT.vector 5 6 7)
        p `sub` v `shouldBe` SUT.point (-2) (-4) (-6)

      {- Scenario: Subtracting two vectors
           Given v1 ← vector(3, 2, 1)
             And v2 ← vector(5, 6, 7)
           Then v1 - v2 = vector(-2, -4, -6) -}
      it "subtracts two vectors" $ do
        let v1 = (SUT.vector 3 2 1)
            v2 = (SUT.vector 5 6 7)
        v1 `sub` v2 `shouldBe` SUT.vector (-2) (-4) (-6)

      {- Scenario: Subtracting a vector from the zero vector
           Given zero ← vector(0, 0, 0)
             And v ← vector(1, -2, 3)
           Then zero - v = vector(-1, 2, -3) -}
      it "subtracts a vector from the zero vector" $ do
        let zero = (SUT.vector 0 0 0)
            v    = (SUT.vector 1 (-2) 3)
        zero `sub` v `shouldBe` SUT.vector (-1) 2 (-3)

    describe "Neg" $ do
      {- Scenario: Negating a tuple
           Given a ← tuple(1, -2, 3, -4)
           Then -a = tuple(-1, 2, -3, 4) -}
      it "negates a tuple" $ do
        let a = (SUT.Tuple 1 (-2) 3 (-4))
        neg a `shouldBe` SUT.Tuple (-1) 2 (-3) 4
        
    describe "Mul" $ do
      {- Scenario: Multiplying a tuple by a scalar
           Given a ← tuple(1, -2, 3, -4)
           Then a * 3.5 = tuple(3.5, -7, 10.5, -14) -}
      it "multiplies a tuple by a scalar" $ do
        let a = (SUT.Tuple 1 (-2) 3 (-4))
        mul a 3.5 `shouldBe` SUT.Tuple 3.5 (-7) 10.5 (-14)

      {- Scenario: Multiplying a tuple by a fraction
           Given a ← tuple(1, -2, 3, -4)
           Then a * 0.5 = tuple(0.5, -1, 1.5, -2) -}
      it "multiplies a tuple by a fraction" $ do
        let a = (SUT.Tuple 1 (-2) 3 (-4))
        mul a 0.5 `shouldBe` SUT.Tuple 0.5 (-1) 1.5 (-2)
        
  --pendingWith "Implementation"

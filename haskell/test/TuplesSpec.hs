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
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Tuple" tupleBasics),
    unsafePerformIO (testSpec "Tuple" tupleArithmetic),
    unsafePerformIO (testSpec "Tuple" colorsAreTuples)]]

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

    describe "Div" $ do
      {- Scenario: Dividing a tuple by a scalar
           Given a ← tuple(1, -2, 3, -4)
           Then a / 2 = tuple(0.5, -1, 1.5, -2) -}
      it "divides a tuple by a scalar" $ do
        let a = (SUT.Tuple 1 (-2) 3 (-4))
        SUT.div a 2 `shouldBe` SUT.Tuple 0.5 (-1) 1.5 (-2)

    describe "Mag" $ do
      {- Scenario: Computing the magnitude of vector(1, 0, 0)
           Given v ← vector(1, 0, 0)
           Then magnitude(v) = 1 -}
      it "computes the magnitude of vector(1, 0, 0)" $ do
        mag (SUT.vector 1 0 0) `shouldBe` 1

      {- Scenario: Computing the magnitude of vector(0, 1, 0)
           Given v ← vector(0, 1, 0)
           Then magnitude(v) = 1 -}
      it "computes the magnitude of vector(0, 1, 0)" $ do
        mag (SUT.vector 0 1 0) `shouldBe` 1
        
      {- Scenario: Computing the magnitude of vector(0, 0, 1)
           Given v ← vector(0, 0, 1)
           Then magnitude(v) = 1 -}
      it "computes the magnitude of vector(0, 0, 1)" $ do
        mag (SUT.vector 0 0 1) `shouldBe` 1

      {- Scenario: Computing the magnitude of vector(1, 2, 3)
           Given v ← vector(1, 2, 3)
           Then magnitude(v) = √14 -}
      it "computes the magnitude of vector(1, 2, 3)" $ do
        mag (SUT.vector 1 2 3) `shouldBe` sqrt 14

      {- Scenario: Computing the magnitude of vector(-1, -2, -3)
           Given v ← vector(-1, -2, -3)
           Then magnitude(v) = √14 -}
      it "computes the magnitude of vector(-1, -2, -3)" $ do
        mag (SUT.vector (-1) (-2) (-3)) `shouldBe` sqrt 14

    describe "Norm" $ do
      {- Scenario: Normalizing vector(4, 0, 0) gives (1, 0, 0)
           Given v ← vector(4, 0, 0)
           Then normalize(v) = vector(1, 0, 0) -}
      it "normalize vector(4, 0, 0) gives (1, 0, 0)" $ do
        norm (SUT.vector 4 0 0) `shouldBe` vector 1 0 0

      {- Scenario: Normalizing vector(1, 2, 3)
           Given v ← vector(1, 2, 3)
                                            # vector(1/√14,   2/√14,   3/√14)
           Then normalize(v) = approximately vector(0.26726, 0.53452, 0.80178) -}
      it "normalize vector(1, 2, 3)" $ do
        let d = sqrt 14
            x = (1 / d)
            y = (2 / d)
            z = (3 / d)
        norm (SUT.vector 1 2 3) `shouldBe` vector x y z

      {- Scenario: The magnitude of a normalized vector
           Given v ← vector(1, 2, 3)
             When norm ← normalize(v)
           Then magnitude(norm) = 1 -}
      it "calculate the magnitude of a normalized vector" $ do
        mag (norm (SUT.vector 1 2 3)) `shouldBe` 1

    describe "dot" $ do
      {- Scenario: The dot product of two tuples
           Given a ← vector(1, 2, 3)
             And b ← vector(2, 3, 4)
           Then dot(a, b) = 20 -}
      it "calculates the dot product of two tuples" $ do
        let a = SUT.vector 1 2 3
            b = SUT.vector 2 3 4
        dot a b `shouldBe` 20

    describe "cross" $ do
      let a = SUT.vector 1 2 3
          b = SUT.vector 2 3 4
      {- Scenario: The cross product of two vectors
           Given a ← vector(1, 2, 3)
             And b ← vector(2, 3, 4)
           Then cross(a, b) = vector(-1, 2, -1)
             And cross(b, a) = vector(1, -2, 1) -}
      it "calculated the cross product of two vectors" $ do
        cross a b `shouldBe` SUT.vector (-1) 2 (-1)
      it "and order matters" $ do
        cross b a `shouldBe` SUT.vector 1 (-2) 1

colorsAreTuples :: Spec
colorsAreTuples =
  describe "Colors" $ do
  {- Scenario: Colors are (red, green, blue) tuples
       Given c ← color(-0.5, 0.4, 1.7)
       Then c.red = -0.5
         And c.green = 0.4
         And c.blue = 1.7 -}
    describe "Components" $ do
      it "are red, green, and blue tuples" $ do
        let c = SUT.color (SUT.Red (-0.5)) (SUT.Green 0.4) (SUT.Blue 1.7)
        (red c) `shouldBe` (SUT.Red (-0.5))
        (green c) `shouldBe` (SUT.Green 0.4)
        (blue c) `shouldBe` (SUT.Blue 1.7)

    {- Scenario: Adding colors
         Given c1 ← color(0.9, 0.6, 0.75)
           And c2 ← color(0.7, 0.1, 0.25)
         Then c1 + c2 = color(1.6, 0.7, 1.0) -}
    describe "Add" $ do
      it "adds two colors" $ do
        let c1 = SUT.color (SUT.Red 0.9) (SUT.Green 0.6) (SUT.Blue 0.75)
            c2 = SUT.color (SUT.Red 0.7) (SUT.Green 0.1) (SUT.Blue 0.25)
        (c1 `add` c2) `shouldBe` color (Red 1.6) (Green 0.7) (Blue 1.0)

    {- Scenario: Subtracting colors
         Given c1 ← color(0.9, 0.6, 0.75)
           And c2 ← color(0.7, 0.1, 0.25)
         Then c1 - c2 = color(0.2, 0.5, 0.5) -}
    describe "Sub" $ do
      it "subtracts two colors" $ do
        let c1 = SUT.color (SUT.Red 0.9) (SUT.Green 0.6) (SUT.Blue 0.75)
            c2 = SUT.color (SUT.Red 0.7) (SUT.Green 0.1) (SUT.Blue 0.25)
        (c1 `sub` c2) `shouldBe` color (Red 0.2) (Green 0.5) (Blue 0.5)

    describe "Mul" $ do
      {- Scenario: Multiplying a color by a scalar
           Given c ← color(0.2, 0.3, 0.4)
           Then c * 2 = color(0.4, 0.6, 0.8) -}
      it "multiplies a color by a scalar" $ do
        let c = color (Red 0.2) (Green 0.3) (Blue 0.4)
        (mul c 2) `shouldBe` color (Red 0.4) (Green 0.6) (Blue 0.8)

    describe "MulC" $ do
      {- Scenario: Multiplying colors
           Given c1 ← color(1, 0.2, 0.4)
             And c2 ← color(0.9, 1, 0.1)
           Then c1 * c2 = color(0.9, 0.2, 0.04) -}
      it "multiplies a color by a color" $ do
        let c1 = color (Red 1) (Green 0.2) (Blue 0.4)
            c2 = color (Red 0.9) (Green 1) (Blue 0.1)
        (mulC c1 c2) `shouldBe` color (Red 0.9) (Green 0.2) (Blue 0.04)
          
        
  --pendingWith "Implementation"

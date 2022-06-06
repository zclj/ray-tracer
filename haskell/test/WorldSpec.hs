module WorldSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Lights
import Tuples
import World as SUT

worldTests :: TestTree
worldTests = testGroup "World Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "World" worldBasics)]]

worldBasics :: Spec
worldBasics =
  describe "World Basics" $ do
    {- Scenario: Creating a world
         Given w ← world()
         Then w contains no objects
           And w has no light source -}
    describe "Creating a world" $ do
      let l   = pointLight (point 0 0 0) (Color (Red 1) (Green 1) (Blue 1))
          w   = SUT.World { objects = []
                          , light   = l}
          obj = (objects w)
          wl  = (light w)
      it "world contains no objects" $ do
        obj `shouldBe` []
      it "world contains light source" $ do
        wl `shouldBe` l

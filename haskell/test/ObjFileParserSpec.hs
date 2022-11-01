module ObjFileParserSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Types
import Shapes
import ObjFileParser

objFileParserTests :: TestTree
objFileParserTests = testGroup "OBJ File Parser Tests" [
  testGroup "Specs for"
    [unsafePerformIO (testSpec "OBJ File Parser" objFileParserBasics)]]

objFileParserBasics :: Spec
objFileParserBasics =
  describe "OBJ File Parser" $ do
    {- Scenario: Ignoring unrecognized lines
         Given gibberish ← a file containing:
           """
           There was a young lady named Bright
           who traveled much faster than light.
           She set out one day
           in a relative way,
           and came back the previous night.
           """
         When parser ← parse_obj_file(gibberish)
         Then parser should have ignored 5 lines -}
    describe "Ignoring unrecognized lines" $ do
      let contents = "There was a young lady named Bright\n\
                      \who traveled much faster than light.\n\
                      \She set out one day\n\
                      \in a relative way,\n\
                      \and came back the previous night."
          parser   = parseObjFile contents
      it "parser should have ignored 5 lines" $ do
        length (ignored parser) `shouldBe` 5

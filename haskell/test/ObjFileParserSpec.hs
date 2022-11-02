module ObjFileParserSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Types
import Shapes
import ObjFileParser
import Tuples as T

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
    {- Scenario: Vertex records
         Given file ← a file containing:
         """
         v -1 1 0
         v -1.0000 0.5000 0.0000
         v 1 0 0
         v 1 1 0
         """
         When parser ← parse_obj_file(file)
         Then parser.vertices[1] = point(-1, 1, 0)
           And parser.vertices[2] = point(-1, 0.5, 0)
           And parser.vertices[3] = point(1, 0, 0)
           And parser.vertices[4] = point(1, 1, 0) -}
    describe "Vertex records" $ do
      let contents = "v -1 1 0\n\
                     \v -1.0000 0.5000 0.0000\n\
                     \v 1 0 0\n\
                     \v 1 1 0"
          parser   = parseObjFile contents
      it "parser.vertices[1] = point(-1, 1, 0)" $ do
        getVertex parser 1 `shouldBe` T.point (-1) 1 0
      it "parser.vertices[2] = point(-1, 0.5, 0)" $ do
        getVertex parser 2 `shouldBe` T.point (-1) 0.5 0
      it "parser.vertices[3] = point(1, 0, 0)" $ do
        getVertex parser 3 `shouldBe` T.point 1 0 0
      it "parser.vertices[4] = point(1, 1, 0)" $ do
        getVertex parser 4 `shouldBe` T.point 1 1 0

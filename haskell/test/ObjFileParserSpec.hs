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
    {- Scenario: Parsing triangle faces
         Given file ← a file containing:
         """
         v -1 1 0
         v -1 0 0
         v 1 0 0
         v 1 1 0

         f 1 2 3
         f 1 3 4
         """
         When parser ← parse_obj_file(file)
           And g ← parser.default_group
           And t1 ← first child of g
           And t2 ← second child of g
         Then t1.p1 = parser.vertices[1]
           And t1.p2 = parser.vertices[2]
           And t1.p3 = parser.vertices[3]
           And t2.p1 = parser.vertices[1]
           And t2.p2 = parser.vertices[3]
           And t2.p3 = parser.vertices[4] -}
    describe "Parsing triangle faces" $ do
      let contents = "v -1 1 0\n\
                     \v -1 0 0\n\
                     \v 1 0 0\n\
                     \v 1 1 0\n\
                     \\n\
                     \f 1 2 3\n\
                     \f 1 3 4"
          parser   = parseObjFile contents
          g = group parser
          c = reverse (children g)
          t1 = head c
          t2 = c !! 1
      it "has children" $ do
        length (children g) `shouldBe` 2
      it "t1.p1 = parser.vertices[1]" $ do
        p1 t1 `shouldBe` getVertex parser 1
      it "t1.p2 = parser.vertices[2]" $ do
        p2 t1 `shouldBe` getVertex parser 2
      it "t1.p3 = parser.vertices[3]" $ do
        p3 t1 `shouldBe` getVertex parser 3
      it "t2.p1 = parser.vertices[1]" $ do
        p1 t2 `shouldBe` getVertex parser 1
      it "t2.p2 = parser.vertices[3]" $ do
        p2 t2 `shouldBe` getVertex parser 3
      it "t2.p3 = parser.vertices[4]" $ do
        p3 t2 `shouldBe` getVertex parser 4

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
          parser   = parseObjFileContent contents
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
          parser   = parseObjFileContent contents
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
          parser   = parseObjFileContent contents
          g = groups parser
          c = reverse (children (head g))
          t1 = head c
          t2 = c !! 1
      it "has children" $ do
        length (children (head g)) `shouldBe` 2
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
    {- Scenario: Triangulating polygons
         Given file ← a file containing:
         """
         v -1 1 0
         v -1 0 0
         v 1 0 0
         v 1 1 0
         v 0 2 0

         f 1 2 3 4 5
         """
         When parser ← parse_obj_file(file)
           And g ← parser.default_group
           And t1 ← first child of g
           And t2 ← second child of g
           And t3 ← third child of g
         Then t1.p1 = parser.vertices[1]
           And t1.p2 = parser.vertices[2]
           And t1.p3 = parser.vertices[3]
           And t2.p1 = parser.vertices[1]
           And t2.p2 = parser.vertices[3]
           And t2.p3 = parser.vertices[4]
           And t3.p1 = parser.vertices[1]
           And t3.p2 = parser.vertices[4]
           And t3.p3 = parser.vertices[5] -}
    describe "Triangulating polygons" $ do
      let contents = "v -1 1 0\n\
                     \v -1 0 0\n\
                     \v 1 0 0\n\
                     \v 1 1 0\n\
                     \v 0 2 0\n\
                     \\n\
                     \f 1 2 3 4 5"
          parser   = parseObjFileContent contents
          g = groups parser
          c = reverse (children (head g))
          t1 = head c
          t2 = c !! 1
          t3 = c !! 2
      it "has children" $ do
        length (children (head g)) `shouldBe` 3
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
      it "t3.p1 = parser.vertices[1]" $ do
        p1 t3 `shouldBe` getVertex parser 1
      it "t3.p2 = parser.vertices[4]" $ do
        p2 t3 `shouldBe` getVertex parser 4
      it "t3.p3 = parser.vertices[5]" $ do
        p3 t3 `shouldBe` getVertex parser 5
    {- Scenario: Triangles in groups
         Given file ← the file "triangles.obj"
         When parser ← parse_obj_file(file)
           And g1 ← "FirstGroup" from parser
           And g2 ← "SecondGroup" from parser
           And t1 ← first child of g1
           And t2 ← first child of g2
         Then t1.p1 = parser.vertices[1]
           And t1.p2 = parser.vertices[2]
           And t1.p3 = parser.vertices[3]
           And t2.p1 = parser.vertices[1]
           And t2.p2 = parser.vertices[3]
           And t2.p3 = parser.vertices[4] -}
    describe "Triangles in groups" $ do
      let contents = "v -1 1 0\n\
                     \v -1 0 0\n\
                     \v 1 0 0\n\
                     \v 1 1 0\n\
                     \\n\
                     \g FirstGroup\n\
                     \f 1 2 3\n\
                     \g SecondGroup\n\
                     \f 1 3 4"
          parser   = parseObjFileContent contents
          [_, g1, g2] = reverse (groups parser)
          c1 = reverse (children g1)
          c2 = reverse (children g2)
          t1 = head c1
          t2 = head c2
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
    {- Scenario: Converting an OBJ file to a group
         Given file ← the file "triangles.obj"
           And parser ← parse_obj_file(file)
         When g ← obj_to_group(parser)
         Then g includes "FirstGroup" from parser
           And g includes "SecondGroup" from parser -}
    describe "Converting an OBJ file to a group" $ do
      let contents = "v -1 1 0\n\
                     \v -1 0 0\n\
                     \v 1 0 0\n\
                     \v 1 1 0\n\
                     \\n\
                     \g FirstGroup\n\
                     \f 1 2 3\n\
                     \g SecondGroup\n\
                     \f 1 3 4"
          parser = parseObjFileContent contents
          g      = objToGroup parser
          [g1, g2] = reverse (children g)
          c1 = reverse (children g1)
          c2 = reverse (children g2)
          t1 = head c1
          t2 = head c2
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

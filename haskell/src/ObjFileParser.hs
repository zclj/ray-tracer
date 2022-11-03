module ObjFileParser where

import Tuples as T
import Data.Char
import Types
import Shapes

import Debug.Trace

data Parser = Parser { ignored  :: [String]
                     , vertices :: [Tuple]
                     , group    :: Shape}
              deriving(Show)

getVertex :: Parser -> Int -> Tuple
getVertex (Parser _ v _) idx = v !! (idx - 1)

parseVertex :: String -> Parser -> Parser
parseVertex s p@(Parser i v g) =
  let parts        = words s
      [v1, v2, v3] = tail parts
  in Parser i (v ++ [(T.point (read v1) (read v2) (read v3))]) g

parseTriangle :: String -> Parser -> Parser
parseTriangle s p@(Parser i v g) =
  let parts        = words s
      [v1, v2, v3] = tail parts
      vx1 = (getVertex p ((read v1)::Int))
      vx2 = (getVertex p ((read v2)::Int))
      vx3 = (getVertex p ((read v3)::Int))
  in Parser i v (fst (addChild g (triangle 1 vx1 vx2 vx3)))

parseObjFileEntry :: String -> Parser -> Parser
parseObjFileEntry s p@(Parser i v g) =
  let parts = (words s)
  in if length parts == 0
     then p
     else case head parts of
            "v" -> parseVertex s p
            "f" -> parseTriangle s p
            _   -> Parser (i ++ [s]) v g

parseObjFile :: String -> Parser
parseObjFile s = foldr parseObjFileEntry (Parser [] [] (defaultGroup 1)) (reverse (lines s))

contents = "v -1 1 0\n\
           \v -1.0000 0.5000 0.0000\n\
           \v 1 0 0\n\
           \foo\n\
           \v 1 1 0\n\
           \bar baz\n\
           \f 1 2 3\n\
           \f 1 2 4"

contents2 = "v -1 1 0\n\
            \v -1 0 0\n\
            \v 1 0 0\n\
            \v 1 1 0\n\
            \\n\
            \f 1 2 3\n\
            \f 1 3 4"

parsed = parseObjFile contents
parsed2 = parseObjFile contents2


module ObjFileParser where

import Tuples as T
import Data.Char
import Types
import Shapes

import Debug.Trace

data Parser = Parser { ignored  :: [String]
                     , vertices :: [Tuple]
                     , groups   :: [Shape]
                     , normals  :: [Tuple]}
              deriving(Show)

getVertex :: Parser -> Int -> Tuple
getVertex (Parser _ v _ _) idx = v !! (idx - 1)

getNormal :: Parser -> Int -> Tuple
getNormal (Parser _ _ _ n) idx = n !! (idx - 1)

parseVertex :: String -> Parser -> Parser
parseVertex s p@(Parser i v g n) =
  let parts        = words s
      [v1, v2, v3] = tail parts
  in Parser i (v ++ [(T.point (read v1) (read v2) (read v3))]) g n

parseNormal :: String -> Parser -> Parser
parseNormal s p@(Parser i v g n) =
  let parts        = words s
      [n1, n2, n3] = tail parts
  in Parser i v g (n ++ [(T.vector (read n1) (read n2) (read n3))])

parseTriangle :: String -> Parser -> Parser
parseTriangle s p@(Parser i v groups n) =
  let parts        = words s
      g:gs         = groups
      [v1, v2, v3] = tail parts
      vx1 = (getVertex p ((read v1)::Int))
      vx2 = (getVertex p ((read v2)::Int))
      vx3 = (getVertex p ((read v3)::Int))
      g'  = (fst (addChild g (triangle 1 vx1 vx2 vx3)))
  in Parser i v (g':gs) n

parsePolygon :: String -> Parser -> Parser
parsePolygon s p@(Parser i v g n) =
  let vertices             = tail (words s)
      [v1, v2, v3, v4, v5] = vertices
  in foldr parseTriangle p [ (unwords ["f", v1, v4, v5])
                           , (unwords ["f", v1, v3, v4])
                           , (unwords ["f", v1, v2, v3])]

-- \f 1/1/1 2/2/2 3/3/3 4/4/4"
parseFace :: String -> Parser -> Parser
parseFace s p@(Parser i v g n) =
  let parts = words s
  in if length (tail parts) == 3
     then parseTriangle s p
     else parsePolygon s p

parseGroup :: String -> Parser -> Parser
parseGroup s p@(Parser i v gs n) =
  let parts = words s
  in Parser i v ((defaultGroup ((length gs) + 1)):gs) n

parseObjFileEntry :: String -> Parser -> Parser
parseObjFileEntry s p@(Parser i v g n) =
  let parts = (words s)
  in if length parts == 0
     then p
     else case head parts of
            "v"  -> parseVertex s p
            "f"  -> parseFace s p
            "g"  -> parseGroup s p
            "vn" -> parseNormal s p
            _   -> Parser (i ++ [s]) v g n

parseObjFileContent :: String -> Parser
parseObjFileContent s =
  foldr parseObjFileEntry (Parser [] [] [(defaultGroup 1)] []) (reverse (lines s))

objToGroup :: Parser -> Shape
objToGroup (Parser _ _ gs _) =
  let defaultG = last gs
  in fst (addChildren defaultG (init gs))

parseObjFile :: String -> IO Parser
parseObjFile path =
  do contents <- readFile path
     return (parseObjFileContent contents)

content = "vn 0 0 1\n\
          \vn 0.707 0 -0.707\n\
          \vn 1 2 3"

parser = parseObjFileContent content

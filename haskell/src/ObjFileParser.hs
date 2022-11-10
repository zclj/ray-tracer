module ObjFileParser where

import Tuples as T
import Data.Char
import Types
import Shapes

import Debug.Trace

data Parser = Parser { ignored  :: [String]
                     , vertices :: [Tuple]
                     , groups   :: [Shape]}
              deriving(Show)

getVertex :: Parser -> Int -> Tuple
getVertex (Parser _ v _) idx = v !! (idx - 1)

parseVertex :: String -> Parser -> Parser
parseVertex s p@(Parser i v g) =
  let parts        = words s
      [v1, v2, v3] = tail parts
  in Parser i (v ++ [(T.point (read v1) (read v2) (read v3))]) g

parseTriangle :: String -> Parser -> Parser
parseTriangle s p@(Parser i v groups) =
  let parts        = words s
      g:gs         = groups
      [v1, v2, v3] = tail parts
      vx1 = (getVertex p ((read v1)::Int))
      vx2 = (getVertex p ((read v2)::Int))
      vx3 = (getVertex p ((read v3)::Int))
      g'  = (fst (addChild g (triangle 1 vx1 vx2 vx3)))
  in Parser i v (g':gs)

parsePolygon :: String -> Parser -> Parser
parsePolygon s p@(Parser i v g) =
  let vertices             = tail (words s)
      [v1, v2, v3, v4, v5] = vertices
  in foldr parseTriangle p [ (unwords ["f", v1, v4, v5])
                           , (unwords ["f", v1, v3, v4])
                           , (unwords ["f", v1, v2, v3])]

-- \f 1/1/1 2/2/2 3/3/3 4/4/4"
parseFace :: String -> Parser -> Parser
parseFace s p@(Parser i v g) =
  let parts = words s
  in if length (tail parts) == 3
     then parseTriangle s p
     else parsePolygon s p

parseGroup :: String -> Parser -> Parser
parseGroup s p@(Parser i v gs) =
  let parts = words s
  in Parser i v ((defaultGroup ((length gs) + 1)):gs)

parseObjFileEntry :: String -> Parser -> Parser
parseObjFileEntry s p@(Parser i v g) =
  let parts = (words s)
  in if length parts == 0
     then p
     else case head parts of
            "v" -> parseVertex s p
            "f" -> parseFace s p
            "g" -> parseGroup s p
            _   -> Parser (i ++ [s]) v g

parseObjFileContent :: String -> Parser
parseObjFileContent s =
  foldr parseObjFileEntry (Parser [] [] [(defaultGroup 1)]) (reverse (lines s))

objToGroup :: Parser -> Shape
objToGroup (Parser _ _ gs) =
  let defaultG = last gs
  in fst (addChildren defaultG (init gs))

parseObjFile :: String -> IO Parser
parseObjFile path =
  do contents <- readFile path
     return (parseObjFileContent contents)

content = "v  7.0000 0.0000 12.0000\n\
          \v  4.9700 -4.9700 12.0000\n\
          \v  4.9811 -4.9811 12.4922\n\
          \v  7.0156 0.0000 12.4922\n\
          \g Teapot001\n\
          \f 1/1/1 2/2/2 3/3/3 4/4/4"

parser = parseObjFileContent content

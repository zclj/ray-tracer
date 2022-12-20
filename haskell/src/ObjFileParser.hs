module ObjFileParser where

import Tuples as T
import Types
import Shapes
import Data.List.Split as S

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

-- Faces might have multiple parts :
-- vertex index/texture index/vertex-normal index
-- f 1/1/1 2/2/2 3/3/3 4/4/4"
-- f 1//3 2//1 3//2
parseFace :: String -> Parser -> Parser
parseFace s p@(Parser i v g n) =
  let parts = words s
  in if length (tail parts) == 3
     then parseTriangle s p
     else parsePolygon s p

makeTriangle :: String -> String -> String -> Parser -> Shape
makeTriangle v1 v2 v3 p =
  let vx1 = (getVertex p ((read v1)::Int))
      vx2 = (getVertex p ((read v2)::Int))
      vx3 = (getVertex p ((read v3)::Int))
  in (triangle 1 vx1 vx2 vx3)

makeSmoothTriangle :: String -> String -> String -> String -> String -> String -> Parser -> Shape
makeSmoothTriangle v1 v2 v3 n1 n2 n3 p =
  let vx1 = (getVertex p ((read v1)::Int))
      vx2 = (getVertex p ((read v2)::Int))
      vx3 = (getVertex p ((read v3)::Int))
      nx1 = (getNormal p ((read n1)::Int))
      nx2 = (getNormal p ((read n2)::Int))
      nx3 = (getNormal p ((read n3)::Int))
  in (smoothTriangle 1 vx1 vx2 vx3 nx1 nx2 nx3)

parseTriangle :: String -> Parser -> Parser
parseTriangle s p@(Parser i v groups n) =
  let parts        = words s
      g:gs         = groups
      [f1, f2, f3] = tail parts
      shape        = if (elem '/' f1)
                     then let [[v1, t1, n1], [v2, t2, n2], [v3, t3, n3]] =
                                map (splitOn "/") [f1, f2, f3]
                          in if (n1 == "") || (n2 == "") || (n3 == "")
                             then makeTriangle f1 f2 f3 p
                             else makeSmoothTriangle v1 v2 v3 n1 n2 n3 p
                     else makeTriangle f1 f2 f3 p
      g'           = (fst (addChild g shape))
  in Parser i v (g':gs) n

parsePolygon :: String -> Parser -> Parser
parsePolygon s p@(Parser i v g n) =
  let vertices             = tail (words s)
      [v1, v2, v3, v4, v5] = vertices
  in foldr parseTriangle p [ (unwords ["f", v1, v4, v5])
                           , (unwords ["f", v1, v3, v4])
                           , (unwords ["f", v1, v2, v3])]

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

content = "v 0 1 0\n\
          \v -1 0 0\n\
          \v 1 0 0\n\
          \       \n\
          \vn -1 0 0\n\
          \vn 1 0 0\n\
          \vn 0 1 0\n\
          \       \n\
          \f 1//3 2//1 3//2 \n\
          \f 1/0/3 2/102/1 3/14/2"

parser = parseObjFileContent content

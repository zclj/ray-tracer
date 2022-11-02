module ObjFileParser where

import Tuples
import Data.Char

data Parser = Parser { ignored  :: [String]
                     , vertices :: [Tuple] }
              deriving(Show)

getVertex :: Parser -> Int -> Tuple
getVertex (Parser _ v) idx = v !! (idx - 1)

parseVertex :: String -> Parser -> Parser
parseVertex s p@(Parser i v) =
  let parts        = words s
      [v1, v2, v3] = tail parts
  in Parser i ((point (read v1) (read v2) (read v3)): v)

parseObjFileEntry :: String -> Parser -> Parser
parseObjFileEntry s p@(Parser i v) =
  case head s of
    'v' -> parseVertex s p
    _   -> Parser (s : i) v

parseObjFile :: String -> Parser
parseObjFile s = foldr parseObjFileEntry (Parser [] []) (lines s)

contents = "v -1 1 0\n\
           \v -1.0000 0.5000 0.0000\n\
           \v 1 0 0\n\
           \foo\n\
           \v 1 1 0\n\
           \bar baz"

parsed = parseObjFile contents


module ObjFileParser where

import Tuples

data Parser = Parser { ignored  :: [String]
                     , vertices :: [Tuple] }

parseObjFile :: String -> Parser
parseObjFile s = Parser (lines s) []

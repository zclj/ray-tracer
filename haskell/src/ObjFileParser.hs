module ObjFileParser where

data Parser = Parser { ignored :: [String] }

parseObjFile :: String -> Parser
parseObjFile s = Parser (lines s)

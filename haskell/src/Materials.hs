module Materials
  ( Material (..)
  , material
  )where

import Tuples

data Material = Material { color     :: Color
                         , ambient   :: Double
                         , diffuse   :: Double
                         , specular  :: Double
                         , shininess :: Double }
                deriving (Show, Eq, Ord)

material :: Material
material = Material (Color (Red 1) (Green 1) (Blue 1)) 0.1 0.9 0.9 200.0

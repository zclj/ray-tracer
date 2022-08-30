module Materials
  ( Material (..)
  , defaultMaterial
  )where

import Tuples
import Patterns

data Material = Material { color           :: Color
                         , ambient         :: Double
                         , diffuse         :: Double
                         , specular        :: Double
                         , shininess       :: Double
                         , reflective      :: Double
                         , transparency    :: Double
                         , refractiveIndex :: Double
                         , materialPattern :: Maybe Pattern}
                deriving (Show, Eq, Ord)

defaultMaterial :: Material
defaultMaterial =
  Material (Color (Red 1) (Green 1) (Blue 1)) 0.1 0.9 0.9 200.0 0.0 0.0 1.0 Nothing

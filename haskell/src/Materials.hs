module Materials
  ( Material (..)
  , defaultMaterial
  )where

import Tuples
import Patterns
import Types

defaultMaterial :: Material
defaultMaterial =
  Material (Color (Red 1) (Green 1) (Blue 1)) 0.1 0.9 0.9 200.0 0.0 0.0 1.0 Nothing

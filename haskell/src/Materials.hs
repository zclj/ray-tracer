module Materials
  ( Material (..)
  , material
  )where

data Material = Material { ambient   :: Double
                         , diffuse   :: Double
                         , specular  :: Double
                         , shininess :: Double }
                deriving (Show, Eq, Ord)

material :: Material
material = Material 0.1 0.9 0.9 200.0

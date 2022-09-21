module Types where

import Matrices
import Tuples

{- Supported shapes -}
data Shape = Sphere { id        :: Int
                    , radius    :: Double
                    , transform :: Matrix
                    , material  :: Material }
           | Plane { id        :: Int
                   , transform :: Matrix
                   , material  :: Material }
           | Cube { id        :: Int
                  , transform :: Matrix
                  , material  :: Material }
           | Cylinder { id        :: Int
                      , transform :: Matrix
                      , material  :: Material
                      , minY      :: Double
                      , maxY      :: Double
                      , closed    :: Bool}
            deriving (Show, Eq, Ord)

data Intersection = Intersection
                    { intersectionT      :: Double
                    , intersectionObject :: Shape}
                  deriving (Show, Eq, Ord)

{- Precomputation of intersections -}
data Computation = Computation { t          :: Double
                               , object     :: Shape
                               , point      :: Tuple
                               , eyev       :: Tuple
                               , normalv    :: Tuple
                               , inside     :: Bool
                               , overPoint  :: Tuple
                               , underPoint :: Tuple
                               , reflectv   :: Tuple
                               , n1         :: Double
                               , n2         :: Double}
                 deriving(Show)

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

{- Patterns -}
data PatternShape = Stripes
                  | Gradient
                  | Ring
                  | Checkers
                  | Point
                  deriving(Eq, Show, Ord)

data Pattern = Pattern { a :: Color
                       , b :: Color
                       , patternTransform :: Matrix
                       , patternShape :: PatternShape}
               deriving(Eq, Show, Ord)

data Ray = Ray { origin :: Tuple
               , direction :: Tuple }
           deriving (Eq, Show)

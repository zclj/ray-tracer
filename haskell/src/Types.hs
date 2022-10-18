module Types where

import Matrices
import Tuples

{- Supported shapes -}
data Shape = Sphere { id        :: Int
                    , radius    :: Double
                    , transform :: Matrix
                    , material  :: Material
                    , parent    :: Maybe Shape }
           | Plane { id        :: Int
                   , transform :: Matrix
                   , material  :: Material
                   , parent    :: Maybe Shape }
           | Cube { id        :: Int
                  , transform :: Matrix
                  , material  :: Material
                  , parent    :: Maybe Shape }
           | Cylinder { id        :: Int
                      , transform :: Matrix
                      , material  :: Material
                      , parent    :: Maybe Shape
                      , minY      :: Double
                      , maxY      :: Double
                      , closed    :: Bool }
           | Cone { id        :: Int
                  , transform :: Matrix
                  , material  :: Material
                  , parent    :: Maybe Shape
                  , minY      :: Double
                  , maxY      :: Double
                  , closed    :: Bool}
           | Group { id        :: Int
                   , transform :: Matrix
                   , parent    :: Maybe Shape
                   , children  :: [Shape] }
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

{- Bounds -}
data BoundingBox = BoundingBox { boundMin :: Tuple
                               , boundMax :: Tuple }
                   deriving (Eq, Show)

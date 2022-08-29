module Shapes where

import Matrices
import Materials
import Tuples
import Rays as R
import Data.List (sort, find)
import Patterns

----------------------------------------
-- Sum Type shapes
----------------------------------------

data AShape = ASphere { id               :: Int
                      , asphereRadius    :: Double
                      , asphereTransform :: Matrix
                      , asphereMaterial  :: Material }
            | APlane { id              :: Int
                     , aplaneTransform :: Matrix
                     , aplaneMaterial  :: Material }
            deriving (Show, Eq, Ord)

aShapeTransform :: AShape -> Matrix
aShapeTransform (ASphere _ _ t _) = t
aShapeTransform (APlane _ t _) = t

aShapeMaterial :: AShape -> Material
aShapeMaterial (ASphere _ _ _ m) = m
aShapeMaterial (APlane _ _ m) = m

aNormalAt :: AShape -> Tuple -> Tuple
aNormalAt ASphere {} objectPoint = objectPoint `sub` point 0 0 0
aNormalAt APlane {} _ = vector 0 1 0

aIntersect :: AShape -> Ray -> [Intersection]
aIntersect s@ASphere {} r =
  let sphereToRay  = origin r `sub` Tuples.point 0 0 0
      a            = direction r `dot` direction r
      b            = 2 * (direction r `dot` sphereToRay)
      c            = (sphereToRay `dot` sphereToRay) - 1
      discriminant = b^2 - (4 * a * c)
  in if discriminant < 0
     then []
     else [ Shapes.Intersection (((-b) - sqrt discriminant) / (2 * a)) s
          , Shapes.Intersection (((-b) + sqrt discriminant) / (2 * a)) s]
aIntersect p@APlane {} r =
  if abs(y (direction r)) < epsilon
  then []
  else let t = -y (origin r) / y (direction r)
       in [Intersection t p]

instance IsShape AShape where
  shapeId        = Shapes.id
  shapeTransform = aShapeTransform
  shapeMaterial  = aShapeMaterial
  shapeNormalAt  = aNormalAt
  shapeIntersect = aIntersect

----------------------------------------

class IsShape a where
  shapeId        :: a -> Int
  shapeTransform :: a -> Matrix
  shapeMaterial  :: a -> Material
  shapeNormalAt  :: a -> Tuple -> Tuple
  shapeIntersect :: a -> Ray -> [Intersection]

data Computation = Computation { cT          :: Double
                               , cObject     :: AShape
                               , cPoint      :: Tuple
                               , cEyev       :: Tuple
                               , cNormalv    :: Tuple
                               , cInside     :: Bool
                               , cOverPoint  :: Tuple
                               , cUnderPoint :: Tuple
                               , cReflectv   :: Tuple
                               , cN1         :: Double
                               , cN2         :: Double}
                 deriving(Show)

intersectShapes :: (Ord a, IsShape a) => [a] -> Ray -> [Intersection]
intersectShapes objects r
  = sort $ concatMap (\s -> shapeIntersect s (R.transform r (inverse (shapeTransform s)))) objects

objectNormalAt :: (IsShape a) => a -> Tuple -> Tuple
objectNormalAt s worldPoint =
  let objectPoint  = inverse (shapeTransform s) `mulT` worldPoint
      objectNormal = shapeNormalAt s objectPoint
      worldNormal  = transpose (inverse (shapeTransform s)) `mulT` objectNormal
      worldNormal' = worldNormal {w=0}
  in norm worldNormal'

removeOrAppend :: (IsShape a, Eq a) => [a] -> a -> [a]
removeOrAppend xs i = if (shapeId i) `elem` (map shapeId xs)
                      then filter (\x -> (shapeId x) /= (shapeId i)) xs
                      else xs ++ [i]

refractiveIndexValue :: (IsShape a) => [a] -> Double
refractiveIndexValue shapes =
  if null shapes
  then 1.0
  else refractiveIndex (shapeMaterial (last shapes))

refractive :: [Intersection] -> [AShape] -> Intersection -> (Double, Double) -> (Double, Double)
refractive [] shapes hit (n1, n2)     = (n1, n2)
refractive (i:is) shapes hit (n1, n2) =
  let shapes' = removeOrAppend shapes (intersectionObject i)
  in if hit == i
     then (refractiveIndexValue shapes, refractiveIndexValue shapes')
     else refractive is shapes' hit (n1, n2)

prepareComputations :: Intersection -> Ray -> [Intersection] -> Computation
prepareComputations i r xs =
  let it               = intersectionT i
      po               = position r it
      obj              = intersectionObject i
      normalv          = objectNormalAt obj po
      eyev             = neg (direction r)
      (inside, normal) = if (normalv `dot` eyev) < 0
                         then (True, neg normalv)
                         else (False, normalv)
      (n1, n2)         = refractive xs [] i (0.0, 0.0)
  in Computation { cT          = it
                 , cObject     = obj
                 , cPoint      = po
                 , cEyev       = eyev
                 , cNormalv    = normal
                 , cInside     = inside
                 , cOverPoint  = po `add` (normal `Tuples.mul` epsilon)
                 , cUnderPoint = po `sub` (normal `Tuples.mul` epsilon)
                 , cReflectv   = reflect (direction r) normal
                 , cN1         = n1
                 , cN2         = n2 }


data Intersection = Intersection
                    { intersectionT      :: Double
                    , intersectionObject :: AShape}
                  deriving (Show, Eq, Ord)

-- |The `hit` function returns the first non-negative intersection.
-- Intersections with a negative value are 'behind', positive 'infront'
hit :: [Intersection] -> Maybe (Intersection)
hit xs = find (\(Intersection t _) -> t >= 0) $ sort xs

patternAtShape :: IsShape a => Pattern -> a -> Tuple -> Color
patternAtShape p shape worldPoint =
  let objectPoint  = inverse (shapeTransform shape) `mulT` worldPoint
      patternPoint = inverse (patternTransform p) `mulT` objectPoint
  in patternAt p patternPoint

schlick :: Computation -> Double
schlick c =
  let n      = cN1 c / cN2 c
      cos    = cEyev c `dot` cNormalv c
      sin2_t = n**2 * (1.0 - cos**2)
      cos_t  = sqrt (1.0 - sin2_t)
      r0     = ((cN1 c - cN2 c) / (cN1 c + cN2 c))**2
  in if cN1 c > cN2 c
     then if sin2_t > 1.0
          then 1.0
          else r0 + (1.0 - r0) * ((1 - cos_t)**5)
     else r0 + (1.0 - r0) * ((1 - cos)**5)


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

data Shape = Sphere { id        :: Int
                    , radius    :: Double
                    , transform :: Matrix
                    , material  :: Material }
           | Plane { id        :: Int
                   , transform :: Matrix
                   , material  :: Material }
            deriving (Show, Eq, Ord)

----------------------------------------
-- Defauls
----------------------------------------

defaultSphere :: Int -> Shape
defaultSphere id = Sphere id 1.0 identity defaultMaterial

makeGlassSphere :: Int -> Shape
makeGlassSphere id =
  Sphere id 1.0 identity (defaultMaterial { transparency = 1.0, refractiveIndex = 1.5 })

defaultPlane :: Int -> Shape
defaultPlane id = Plane id identity defaultMaterial

----------------------------------------
localNormalAt :: Shape -> Tuple -> Tuple
localNormalAt Sphere {} objectPoint = objectPoint `sub` point 0 0 0
localNormalAt Plane {} _ = vector 0 1 0

data Computation = Computation { cT          :: Double
                               , cObject     :: Shape
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

localIntersect :: Shape -> Ray -> [Intersection]
localIntersect s@Sphere {} r =
  let sphereToRay  = origin r `sub` Tuples.point 0 0 0
      a            = direction r `dot` direction r
      b            = 2 * (direction r `dot` sphereToRay)
      c            = (sphereToRay `dot` sphereToRay) - 1
      discriminant = b^2 - (4 * a * c)
  in if discriminant < 0
     then []
     else [ Shapes.Intersection (((-b) - sqrt discriminant) / (2 * a)) s
          , Shapes.Intersection (((-b) + sqrt discriminant) / (2 * a)) s]
localIntersect p@Plane {} r =
  if abs(y (direction r)) < epsilon
  then []
  else let t = -y (origin r) / y (direction r)
       in [Intersection t p]

intersectShapes :: [Shape] -> Ray -> [Intersection]
intersectShapes objects r
  = sort $ concatMap (\s -> localIntersect s (R.transform r (inverse (Shapes.transform s)))) objects

objectNormalAt :: Shape -> Tuple -> Tuple
objectNormalAt s worldPoint =
  let objectPoint  = inverse (Shapes.transform s) `mulT` worldPoint
      objectNormal = localNormalAt s objectPoint
      worldNormal  = transpose (inverse (Shapes.transform s)) `mulT` objectNormal
      worldNormal' = worldNormal {w=0}
  in norm worldNormal'

removeOrAppend :: [Shape] -> Shape -> [Shape]
removeOrAppend xs i = if (Shapes.id i) `elem` (map Shapes.id xs)
                      then filter (\x -> (Shapes.id x) /= (Shapes.id i)) xs
                      else xs ++ [i]

refractiveIndexValue :: [Shape] -> Double
refractiveIndexValue shapes =
  if null shapes
  then 1.0
  else refractiveIndex (Shapes.material (last shapes))

refractive :: [Intersection] -> [Shape] -> Intersection -> (Double, Double) -> (Double, Double)
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
                    , intersectionObject :: Shape}
                  deriving (Show, Eq, Ord)

-- |The `hit` function returns the first non-negative intersection.
-- Intersections with a negative value are 'behind', positive 'infront'
hit :: [Intersection] -> Maybe (Intersection)
hit xs = find (\(Intersection t _) -> t >= 0) $ sort xs

patternAtShape :: Pattern -> Shape -> Tuple -> Color
patternAtShape p shape worldPoint =
  let objectPoint  = inverse (Shapes.transform shape) `mulT` worldPoint
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


module Shapes where

import Matrices
import Materials
import Tuples as T
import Rays as R
import Data.List (sort, find)
import Patterns
import Types

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
localNormalAt Sphere {} objectPoint = objectPoint `sub` T.point 0 0 0
localNormalAt Plane {} _ = vector 0 1 0

localIntersect :: Shape -> Ray -> [Intersection]
localIntersect s@Sphere {} r =
  let sphereToRay  = origin r `sub` T.point 0 0 0
      a            = direction r `dot` direction r
      b            = 2 * (direction r `dot` sphereToRay)
      c            = (sphereToRay `dot` sphereToRay) - 1
      discriminant = b^2 - (4 * a * c)
  in if discriminant < 0
     then []
     else [ Intersection (((-b) - sqrt discriminant) / (2 * a)) s
          , Intersection (((-b) + sqrt discriminant) / (2 * a)) s]
localIntersect p@Plane {} r =
  if abs(y (direction r)) < epsilon
  then []
  else let t = -y (origin r) / y (direction r)
       in [Intersection t p]

intersectShapes :: [Shape] -> Ray -> [Intersection]
intersectShapes objects r
  = sort $ concatMap (\s -> localIntersect s (R.transform r (inverse (Types.transform s)))) objects

objectNormalAt :: Shape -> Tuple -> Tuple
objectNormalAt s worldPoint =
  let objectPoint  = inverse (Types.transform s) `mulT` worldPoint
      objectNormal = localNormalAt s objectPoint
      worldNormal  = transpose (inverse (Types.transform s)) `mulT` objectNormal
      worldNormal' = worldNormal {w=0}
  in norm worldNormal'

removeOrAppend :: [Shape] -> Shape -> [Shape]
removeOrAppend xs i = if (Types.id i) `elem` (map Types.id xs)
                      then filter (\x -> (Types.id x) /= (Types.id i)) xs
                      else xs ++ [i]

refractiveIndexValue :: [Shape] -> Double
refractiveIndexValue shapes =
  if null shapes
  then 1.0
  else refractiveIndex (Types.material (last shapes))

refractive :: [Intersection] -> [Shape] -> Intersection -> (Double, Double) -> (Double, Double)
refractive [] shapes hit (n1, n2)     = (n1, n2)
refractive (i:is) shapes hit (n1, n2) =
  let shapes' = removeOrAppend shapes (intersectionObject i)
  in if hit == i
     then (refractiveIndexValue shapes, refractiveIndexValue shapes')
     else refractive is shapes' hit (n1, n2)

-- prepareComputations :: Intersection -> Ray -> [Intersection] -> Computation
-- prepareComputations i r xs =
--   let it               = intersectionT i
--       po               = position r it
--       obj              = intersectionObject i
--       normalv          = objectNormalAt obj po
--       eyev             = neg (direction r)
--       (inside, normal) = if (normalv `dot` eyev) < 0
--                          then (True, neg normalv)
--                          else (False, normalv)
--       (n1, n2)         = refractive xs [] i (0.0, 0.0)
--   in Computation { t          = it
--                  , object     = obj
--                  , Types.point = po
--                  , eyev       = eyev
--                  , normalv    = normal
--                  , inside     = inside
--                  , overPoint  = po `add` (normal `T.mul` epsilon)
--                  , underPoint = po `sub` (normal `T.mul` epsilon)
--                  , reflectv   = reflect (direction r) normal
--                  , n1         = n1
--                  , n2         = n2 }


-- data Intersection = Intersection
--                     { intersectionT      :: Double
--                     , intersectionObject :: Shape}
--                   deriving (Show, Eq, Ord)



patternAtShape :: Pattern -> Shape -> Tuple -> Color
patternAtShape p shape worldPoint =
  let objectPoint  = inverse (Types.transform shape) `mulT` worldPoint
      patternPoint = inverse (patternTransform p) `mulT` objectPoint
  in patternAt p patternPoint


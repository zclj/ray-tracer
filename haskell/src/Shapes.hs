module Shapes where

import Matrices
import Materials
import Tuples as T
import Rays as R
import Data.List (sort, find)
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

defaultCube :: Int -> Shape
defaultCube id = Cube id identity defaultMaterial

----------------------------------------
cubeNormal :: Double -> Double -> Double -> Double -> Tuple
cubeNormal m x y z
  | m == abs x = vector x 0 0
  | m == abs y = vector 0 y 0
  | m == abs z = vector 0 0 z

localNormalAt :: Shape -> Tuple -> Tuple
localNormalAt Sphere {} objectPoint = objectPoint `sub` T.point 0 0 0
localNormalAt Plane {} _ = vector 0 1 0
localNormalAt Cube {} objectPoint@(Tuple x y z _) =
  let maxc = maximum [abs x, abs y, abs z]
  in cubeNormal maxc x y z

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
localIntersect c@Cube {} r =
  let (xtmin, xtmax) = checkAxis (x (origin r)) (x (direction r))
      (ytmin, ytmax) = checkAxis (y (origin r)) (y (direction r))
      (ztmin, ztmax) = checkAxis (z (origin r)) (z (direction r))
      tmin = maximum [xtmin, ytmin, ztmin]
      tmax = minimum [xtmax, ytmax, ztmax]
  in if tmin > tmax
     then []
     else [Intersection tmin c, Intersection tmax c]

checkAxis :: Double -> Double -> (Double, Double)
checkAxis origin direction =
  let tmin = ((-1) - origin) / direction
      tmax = (1 - origin) / direction
  in if tmin > tmax
     then (tmax, tmin)
     else (tmin, tmax)

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

patternAtShape :: Pattern -> Shape -> Tuple -> Color
patternAtShape p shape worldPoint =
  let objectPoint  = inverse (Types.transform shape) `mulT` worldPoint
      patternPoint = inverse (patternTransform p) `mulT` objectPoint
  in patternAt p patternPoint


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

defaultCylinder :: Int -> Shape
defaultCylinder id = Cylinder id identity defaultMaterial ((-1)/0) (1/0) False

defaultCone :: Int -> Shape
defaultCone id = Cone id identity defaultMaterial ((-1)/0) (1/0) False

----------------------------------------
cubeNormal :: Double -> Double -> Double -> Double -> Tuple
cubeNormal m x y z
  | m ~= abs x = vector x 0 0
  | m ~= abs y = vector 0 y 0
  | m ~= abs z = vector 0 0 z

localNormalAt :: Shape -> Tuple -> Tuple
localNormalAt Sphere {} objectPoint = objectPoint `sub` T.point 0 0 0
localNormalAt Plane {} _ = vector 0 1 0
localNormalAt Cube {} objectPoint@(Tuple x y z _) =
  let maxc = maximum [abs x, abs y, abs z]
  in cubeNormal maxc x y z
localNormalAt c@Cylinder {} objectPoint@(Tuple x y z _)
  | (x**2 + z**2) < 1 && (y >= ((maxY c) - epsilon)) = vector 0 1 0
  | (x**2 + z**2) < 1 && (y <= ((minY c) + epsilon)) = vector 0 (-1) 0
  | otherwise = vector x 0 z
localNormalAt c@Cone {} objectPoint@(Tuple x y z _)
  | (x**2 + z**2) < 1 && (y >= ((maxY c) - epsilon)) = vector 0 1 0
  | (x**2 + z**2) < 1 && (y <= ((minY c) + epsilon)) = vector 0 (-1) 0
  | otherwise = let yn  = sqrt(x^2 + z^2)
                    yn' = if y > 0 then -yn else yn
                in vector x yn' z

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
localIntersect cy@Cylinder {} r =
  let a = ((x (direction r))**2) + ((z (direction r))**2)
      b = ((2 * (x (origin r))) * (x (direction r))) +
          ((2 * (z (origin r))) * (z (direction r)))
      c = ((x (origin r))**2) + ((z (origin r))**2) - 1
  in if a ~= 0
     then intersectCaps cy r
     else let bi = intersectBody cy a b c r
          in bi ++ (intersectCaps cy r)
localIntersect cone@Cone {} r =
  let a = ((x (direction r))^2) - ((y (direction r))^2) + ((z (direction r))^2)
      b = ((2 * (x (origin r))) * (x (direction r))) -
          ((2 * (y (origin r))) * (y (direction r))) +
          ((2 * (z (origin r))) * (z (direction r)))
      c = ((x (origin r))^2) - ((y (origin r))^2) + ((z (origin r))^2)
  in if a ~= 0 && b ~= 0
     then []
     else if a ~= 0
          then [Intersection (- c /(2*b)) cone] ++ (intersectCaps cone r)
          else let bi = intersectBody cone a b c r
               in bi ++ (intersectCaps cone r)

intersectBody :: Shape -> Double -> Double -> Double -> Ray -> [Intersection]
intersectBody s a b c r
  = let disc = (b**2) - (4 * a * c)
        t0   = ((-b) - (sqrt disc)) / (2 * a)
        t1   = ((-b) + (sqrt disc)) / (2 * a)
        (t0', t1') = if t0 > t1 then (t1, t0) else (t0, t1)
        y0 = (y (origin r)) + t0' * (y (direction r))
        y1 = (y (origin r)) + t1' * (y (direction r))
        i0 = [Intersection t0' s | (minY s) < y0 && y0 < (maxY s)]
        i1 = [Intersection t1' s | (minY s) < y1 && y1 < (maxY s)]
    in i0 ++ i1

intersectCaps :: Shape -> Ray -> [Intersection]
intersectCaps s r
  = if (not (closed s)) || (y (direction r)) ~= 0
    then []
    else let tmin = ((minY s) - (y (origin r))) / (y (direction r))
             tmax = ((maxY s) - (y (origin r))) / (y (direction r))
             (rmin, rmax) = case s of
                              Cylinder {} -> (1,1)
                              Cone {}     -> ((minY s), (maxY s))
             imin = [Intersection tmin s | checkCap r tmin rmin]
             imax = [Intersection tmax s | checkCap r tmax rmax]
         in imin ++ imax

checkCap :: Ray -> Double -> Double -> Bool
checkCap r t y = let x'  = (x (origin r)) + (t * (x (direction r)))
                     z'  = (z (origin r)) + (t * (z (direction r)))
                 in (x'^2 + z'^2) <= y^2

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
removeOrAppend xs i = if Types.id i `elem` map Types.id xs
                      then filter (\x -> Types.id x /= Types.id i) xs
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


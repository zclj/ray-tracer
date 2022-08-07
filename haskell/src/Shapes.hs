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
                      , asphereTransform :: VMatrix
                      , asphereMaterial  :: Material }
            | APlane { id              :: Int
                     , aplaneTransform :: VMatrix
                     , aplaneMaterial  :: Material }
            deriving (Show, Eq, Ord)

aShapeTransform :: AShape -> VMatrix
aShapeTransform (ASphere _ _ t _) = t
aShapeTransform (APlane _ t _) = t

aShapeMaterial :: AShape -> Material
aShapeMaterial (ASphere _ _ _ m) = m
aShapeMaterial (APlane _ _ m) = m

aNormalAt :: AShape -> Tuple -> Tuple
aNormalAt (ASphere _ _ _ _) objectPoint = objectPoint `sub` point 0 0 0
aNormalAt (APlane _ _ _) _ = vector 0 1 0

aIntersect :: AShape -> Ray -> [Intersection AShape]
aIntersect s@(ASphere _ _ _ _) r =
  let sphereToRay  = origin r `sub` Tuples.point 0 0 0
      a            = direction r `dot` direction r
      b            = 2 * (direction r `dot` sphereToRay)
      c            = (sphereToRay `dot` sphereToRay) - 1
      discriminant = b^2 - (4 * a * c)
  in if discriminant < 0
     then []
     else [ Shapes.Intersection (((-b) - sqrt discriminant) / (2 * a)) s
          , Shapes.Intersection (((-b) + sqrt discriminant) / (2 * a)) s]
aIntersect p@(APlane _ _ _) r =
  if abs(y (direction r)) < epsilon
  then []
  else let t = -y (origin r) / y (direction r)
       in [Intersection t p]

instance IsShape AShape where
  shapeTransform = aShapeTransform
  shapeMaterial  = aShapeMaterial
  shapeNormalAt  = aNormalAt
  shapeIntersect = aIntersect

----------------------------------------

class IsShape a where
  shapeTransform :: a -> VMatrix
  shapeMaterial  :: a -> Material
  shapeNormalAt  :: a -> Tuple -> Tuple
  shapeIntersect :: a -> Ray -> [Intersection a]

data Computation a = Computation { cT         :: Double
                                 , cObject    :: a
                                 , cPoint     :: Tuple
                                 , cEyev      :: Tuple
                                 , cNormalv   :: Tuple
                                 , cInside    :: Bool
                                 , cOverPoint :: Tuple
                                 , cReflectv  :: Tuple
                                 , cN1        :: Double
                                 , cN2        :: Double}
                   deriving(Show)

intersectShapes :: (Ord a, IsShape a) => [a] -> Ray -> [Intersection a]
intersectShapes objects r
  = sort $ concatMap (\s -> shapeIntersect s (R.transform r (inverseV (shapeTransform s)))) objects

objectNormalAt :: (IsShape a) => a -> Tuple -> Tuple
objectNormalAt s worldPoint =
  let objectPoint  = inverseV (shapeTransform s) `mulTV` worldPoint
      objectNormal = shapeNormalAt s objectPoint
      worldNormal  = transposeV (inverseV (shapeTransform s)) `mulTV` objectNormal
      worldNormal' = worldNormal {w=0}
  in norm worldNormal'

-- containers ← empty list --/ seems this list should contain refractive_index

-- for i ← each intersection in xs
--   if i = hit then
--     if containers is empty
--       comps.n1 ← 1.0
--     else
--       comps.n1 ← last(containers).material.refractive_index
--     end if
--   end if

--   if containers includes i.object then
--     remove i.object from containers --/ id should be enough?
--   else
--     append i.object onto containers
--   end if

--   if i = hit then
--     if containers is empty
--       comps.n2 ← 1.0
--     else
--       comps.n2 ← last(containers).material.refractive_index
--     end if

--     terminate loop
--   end if
-- end for

refractive :: (IsShape a, Ord a) => Intersection a -> (Double, Double)
refractive i = let aHit       = hit [i]
                   containers = []::[a]
               in case aHit of
                    Nothing  -> (0,0)
                    Just (h) -> if null containers
                                then (1.0, 0.0)
                                else (1.0, 0.0)--(refractiveIndex (shapeMaterial (last containers)), 0.0)

refractiveIndices :: (IsShape a, Ord a) => [Intersection a] -> (Double, Double)
refractiveIndices xs =
  head $ map (\i -> refractive i) xs

prepareComputations ::
  (IsShape a, Ord a) => Intersection a -> Ray -> [Intersection a] -> Computation a
prepareComputations i r xs =
  let it               = intersectionT i
      po               = position r it
      obj              = intersectionObject i
      normalv          = objectNormalAt obj po
      eyev             = neg (direction r)
      (inside, normal) = if (normalv `dot` eyev) < 0
                         then (True, neg normalv)
                         else (False, normalv)
      (n1, n2)         = refractiveIndices xs
  in Computation { cT         = it
                 , cObject    = obj
                 , cPoint     = po
                 , cEyev      = eyev
                 , cNormalv   = normal
                 , cInside    = inside
                 , cOverPoint = po `add` (normal `Tuples.mul` epsilon)
                 , cReflectv  = reflect (direction r) normal
                 , cN1        = n1
                 , cN2        = n2 }


data Intersection a = Intersection
                      { intersectionT      :: Double
                      , intersectionObject :: a}
                    deriving (Show, Eq, Ord)

-- |The `hit` function returns the first non-negative intersection.
-- Intersections with a negative value are 'behind', positive 'infront'
hit :: (IsShape a, Ord a) => [Intersection a] -> Maybe (Intersection a)
hit xs = find (\(Intersection t _) -> t >= 0) $ sort xs

patternAtShape :: IsShape a => Pattern -> a -> Tuple -> Color
patternAtShape p shape worldPoint =
  let objectPoint  = inverseV (shapeTransform shape) `mulTV` worldPoint
      patternPoint = inverseV (patternTransform p) `mulTV` objectPoint
  in patternAt p patternPoint

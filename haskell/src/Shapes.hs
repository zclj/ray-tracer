{-# LANGUAGE NamedFieldPuns #-}

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
defaultSphere id = Sphere id 1.0 identity defaultMaterial Nothing

makeGlassSphere :: Int -> Shape
makeGlassSphere id =
  Sphere id 1.0 identity (defaultMaterial { transparency = 1.0, refractiveIndex = 1.5 }) Nothing

defaultPlane :: Int -> Shape
defaultPlane id = Plane id identity defaultMaterial Nothing

defaultCube :: Int -> Shape
defaultCube id = Cube id identity defaultMaterial Nothing

defaultCylinder :: Int -> Shape
defaultCylinder id = Cylinder id identity defaultMaterial Nothing ((-1)/0) (1/0) False

defaultCone :: Int -> Shape
defaultCone id = Cone id identity defaultMaterial Nothing ((-1)/0) (1/0) False

defaultGroup :: Int -> Shape
defaultGroup id = Group id identity Nothing []

triangle :: Int -> Tuple -> Tuple -> Tuple -> Shape
triangle id p1 p2 p3 =
  let e1 = (p2 `sub` p1)
      e2 = (p3 `sub` p1)
      n  = norm (e2 `cross` e1)
  in Triangle id identity defaultMaterial p1 p2 p3 e1 e2 n Nothing

smoothTriangle :: Int -> Tuple -> Tuple -> Tuple -> Tuple -> Tuple -> Tuple -> Shape
smoothTriangle id p1 p2 p3 n1 n2 n3
  = let e1 = (p2 `sub` p1)
        e2 = (p3 `sub` p1)
    in SmoothTriangle id identity defaultMaterial p1 p2 p3 e1 e2 n1 n2 n3 Nothing

----------------------------------------
cubeNormal :: Double -> Double -> Double -> Double -> Tuple
cubeNormal m x y z
  | m ~= abs x = vector x 0 0
  | m ~= abs y = vector 0 y 0
  | m ~= abs z = vector 0 0 z

localNormalAt :: Shape -> Tuple -> Intersection -> Tuple
localNormalAt Sphere {} objectPoint _ = objectPoint `sub` T.point 0 0 0
localNormalAt Plane {} _ _ = vector 0 1 0
localNormalAt Cube {} objectPoint@(Tuple x y z _) _ =
  let maxc = maximum [abs x, abs y, abs z]
  in cubeNormal maxc x y z
localNormalAt c@Cylinder {} objectPoint@(Tuple x y z _) _
  | (x**2 + z**2) < 1 && (y >= ((maxY c) - epsilon)) = vector 0 1 0
  | (x**2 + z**2) < 1 && (y <= ((minY c) + epsilon)) = vector 0 (-1) 0
  | otherwise = vector x 0 z
localNormalAt c@Cone {} objectPoint@(Tuple x y z _) _
  | (x**2 + z**2) < 1 && (y >= ((maxY c) - epsilon)) = vector 0 1 0
  | (x**2 + z**2) < 1 && (y <= ((minY c) + epsilon)) = vector 0 (-1) 0
  | otherwise = let yn  = sqrt(x^2 + z^2)
                    yn' = if y > 0 then -yn else yn
                in vector x yn' z
localNormalAt t@Triangle {} _ _ = normal t
localNormalAt t@SmoothTriangle {} point hit =
  ((tn2 t) `T.mul` (intersectionU hit)) `T.add`
  ((tn3 t) `T.mul` (intersectionV hit)) `T.add`
  ((tn1 t) `T.mul` (1 - (intersectionU hit) - (intersectionV hit)))

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
  let (xtmin, xtmax) = checkAxis (x (origin r)) (x (direction r)) (-1) 1
      (ytmin, ytmax) = checkAxis (y (origin r)) (y (direction r)) (-1) 1
      (ztmin, ztmax) = checkAxis (z (origin r)) (z (direction r)) (-1) 1
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
localIntersect group@Group {children} r =
  if intersectBox (bounds group) r
  then intersectShapes children r
  else []
localIntersect t@Triangle {} r = intersectTriangle t r
localIntersect t@SmoothTriangle {} r = intersectTriangle t r

intersectTriangle :: Shape -> Ray -> [Intersection]
intersectTriangle t r =
  let dirCrossE2 = (direction r) `cross` (e2 t)
      det        = (e1 t) `dot` dirCrossE2
  in if (abs det) < epsilon
     then []
     else let f = 1.0 / det
              p1ToOrigin = (origin r) `sub` (p1 t)
              u = f * (p1ToOrigin `dot` dirCrossE2)
          in if u < 0 || u > 1
             then []
             else let originCrossE1 = p1ToOrigin `cross` (e1 t)
                      v = f * ((direction r) `dot` originCrossE1)
                  in if v < 0 || (u + v) > 1
                     then []
                     else let tt = f * (e2 t) `dot` originCrossE1
                          in case t of
                               Triangle {} -> [Intersection tt t]
                               SmoothTriangle {} -> [IntersectionUV tt t u v]

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

checkAxis :: Double -> Double -> Double -> Double -> (Double, Double)
checkAxis origin direction min max =
  let tmin = (min - origin) / direction
      tmax = (max - origin) / direction
  in if tmin > tmax
     then (tmax, tmin)
     else (tmin, tmax)

intersectShapes :: [Shape] -> Ray -> [Intersection]
intersectShapes objects r
  = sort $ concatMap (\s -> localIntersect s (R.transform r (inverse (Types.transform s)))) objects

objectNormalAt :: Shape -> Tuple -> Intersection -> Tuple
objectNormalAt s worldPoint i =
  let localPoint  = worldToObject s worldPoint
      localNormal = localNormalAt s localPoint i
      worldNormal = normalToWorld s localNormal
  in worldNormal

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
  let objectPoint  = worldToObject shape worldPoint
      patternPoint = inverse (patternTransform p) `mulT` objectPoint
  in patternAt p patternPoint

{- Group -}
addChild :: Shape -> Shape -> (Shape, Shape)
addChild group@Group {children} child =
  let newShape = child { parent = Just group }
      newGroup = group { children = newShape : children }
  in (newGroup, newShape)

addChildren :: Shape -> [Shape] -> (Shape, [Shape])
addChildren group children = foldr
                             (\c (g, cs) -> let (g', c') = addChild g c in (g', c':cs))
                             (group, []) children

partitionChild :: BoundingBox -> BoundingBox -> ([Shape], [Shape], [Shape]) -> Shape
  -> ([Shape], [Shape], [Shape])
partitionChild leftBound rightBound (outsidePart, leftPart, rightPart) child =
  let bounds = parentSpaceBoundsOf child
  in if (boxContainsBox leftBound bounds)
     then (outsidePart, child:leftPart, rightPart)
     else if (boxContainsBox rightBound bounds)
          then (outsidePart, leftPart, child:rightPart)
          else (child:outsidePart, leftPart, rightPart)

partitionChildren :: Shape -> (Shape, [Shape], [Shape])
partitionChildren group =
  -- split the groups bounding box
  let groupBounds = bounds group
      (left, right) = splitBounds groupBounds
      partitionWith = partitionChild left right
  -- make a group for the shapes that fit in the 'left' or 'right' group
      (out, l, r) = foldr(\c acc -> partitionWith acc c) ([],[],[]) (children group)
  in (group { children = out }, l, r)

makeSubgroup :: Shape -> [Shape] -> Shape
-- make a new group and add the group with the children to that group
makeSubgroup group children =
  let subgroup        = (defaultGroup ((Types.id group) + 1))
      (subgroup', cs) = addChildren subgroup children
      (group', _)     = addChildren group [subgroup']
  in group'

updateGroupParents :: Shape -> (Shape -> Shape) -> Shape
updateGroupParents group@Group{} update =
  let updated = update group
   in updated {children = map (`updateGroup` update) (children updated)}
updateGroup primitive update =
  case parent primitive of
    Nothing -> primitive
    Just p -> primitive {parent = Just (update p)}

updateTransform :: Shape -> Matrix -> Shape
updateTransform shape t =
  updateGroupParents shape (\s -> s { Types.transform = t })

updateMaterial :: Shape -> Material -> Shape
updateMaterial shape m =
  updateGroupParents shape (\s -> s { material = m })

worldToObject :: Shape -> Tuple -> Tuple
worldToObject s point =
  case (parent s) of
    Nothing -> inverse (Types.transform s) `mulT` point
    Just p  -> let objPoint = worldToObject p point
               in inverse (Types.transform s) `mulT` objPoint

normalToWorld :: Shape -> Tuple -> Tuple
normalToWorld s objectNormal =
  let normal     = (transpose (inverse (Types.transform s)) `mulT` objectNormal) { w = 0 }
      normalized = norm normal
  in case (parent s) of
       Nothing -> normalized
       Just p  -> normalToWorld p normalized

{- Bounds -}

bounds :: Shape -> BoundingBox
bounds shape@Sphere {} = BoundingBox (T.point (-1) (-1) (-1)) (T.point 1 1 1)

bounds shape@Plane {} = BoundingBox (T.point (-1/0) 0 (-1/0)) (T.point (1/0) 0 (1/0))

bounds shape@Cube {} = BoundingBox (T.point (-1) (-1) (-1)) (T.point 1 1 1)

bounds shape@Cylinder {minY, maxY}
  = BoundingBox (T.point (-1) minY (-1)) (T.point 1 maxY 1)

bounds shape@Cone {minY, maxY}
  = let absMin = abs minY
        absMax = abs maxY
        limit  = max absMin absMax
    in BoundingBox (T.point (-limit) minY (-limit)) (T.point limit maxY limit)

bounds shape@Group { children } =
  foldr (\c b -> addBoxes b (parentSpaceBoundsOf c)) defaultBoundingBox children

bounds shape@Triangle { p1, p2, p3 } =
  foldr (flip addBoundingBoxPoint) defaultBoundingBox [p1, p2, p3]

defaultBoundingBox :: BoundingBox
defaultBoundingBox = BoundingBox (T.point (1/0) (1/0) (1/0)) (T.point (-1/0) (-1/0) (-1/0))

addBoundingBoxPoint :: BoundingBox -> Tuple -> BoundingBox
addBoundingBoxPoint (BoundingBox boundMin boundMax) point =
  let minX = if (x point) < (x boundMin) then (x point) else (x boundMin)
      minY = if (y point) < (y boundMin) then (y point) else (y boundMin)
      minZ = if (z point) < (z boundMin) then (z point) else (z boundMin)
      maxX = if (x point) > (x boundMax) then (x point) else (x boundMax)
      maxY = if (y point) > (y boundMax) then (y point) else (y boundMax)
      maxZ = if (z point) > (z boundMax) then (z point) else (z boundMax)
  in BoundingBox (T.point minX minY minZ) (T.point maxX maxY maxZ)

addBoxes :: BoundingBox -> BoundingBox -> BoundingBox
addBoxes a b = (a `addBoundingBoxPoint` (boundMin b)) `addBoundingBoxPoint` (boundMax b)

boxContainsPoint :: BoundingBox -> Tuple -> Bool
boxContainsPoint (BoundingBox boundMin boundMax) point
  = (x point) >= (x boundMin) && (x point) <= (x boundMax)
    && (y point) >= (y boundMin) && (y point) <= (y boundMax)
    && (z point) >= (z boundMin) && (z point) <= (z boundMax)

boxContainsBox :: BoundingBox -> BoundingBox -> Bool
boxContainsBox a (BoundingBox boundMin boundMax)
  = boxContainsPoint a boundMin && boxContainsPoint a boundMax

transformBox :: BoundingBox -> Matrix -> BoundingBox
transformBox (BoundingBox boundMin boundMax) m =
  let p1 = boundMin
      p2 = T.point (x boundMin) (y boundMin) (z boundMax)
      p3 = T.point (x boundMin) (y boundMax) (z boundMin)
      p4 = T.point (x boundMin) (y boundMax) (z boundMax)
      p5 = T.point (x boundMax) (y boundMin) (z boundMin)
      p6 = T.point (x boundMax) (y boundMin) (z boundMax)
      p7 = T.point (x boundMax) (y boundMax) (z boundMin)
      p8 = boundMax
  in foldr
     (\p box -> addBoundingBoxPoint box (m `mulT` p))
     defaultBoundingBox
     [p1, p2, p3, p4, p5, p6, p7, p8]

parentSpaceBoundsOf :: Shape -> BoundingBox
parentSpaceBoundsOf s = transformBox (bounds s) (Types.transform s)

intersectBox :: BoundingBox -> Ray -> Bool
-- test with the unit cube. No intersections == miss
intersectBox (BoundingBox boundMin boundMax) r =
  let (xtmin, xtmax) = checkAxis (x (origin r)) (x (direction r)) (x boundMin) (x boundMax)
      (ytmin, ytmax) = checkAxis (y (origin r)) (y (direction r)) (y boundMin) (y boundMax)
      (ztmin, ztmax) = checkAxis (z (origin r)) (z (direction r)) (z boundMin) (z boundMax)
      tmin = maximum [xtmin, ytmin, ztmin]
      tmax = minimum [xtmax, ytmax, ztmax]
  in if tmin > tmax
     then False
     else True

splitBounds :: BoundingBox -> (BoundingBox, BoundingBox)
splitBounds b@(BoundingBox boundMin boundMax) =
  -- find largest dimension
  let dx = abs ((x boundMax) - (x boundMin))
      dy = abs ((y boundMax) - (y boundMin))
      dz = abs ((z boundMax) - (z boundMin))
      greatest = maximum [dx, dy, dz]
      (x0, y0, z0) = ((x boundMin), (y boundMin), (z boundMin))
      (x1, y1, z1) = ((x boundMax), (y boundMax), (z boundMax))
      (x2, y2, z2, x3, y3, z3) = if greatest == dx
                                 then (((x boundMin) + dx / 2), y0, z0, ((x boundMin) + dx / 2), y1, z1)
                                 else if greatest == dy
                                      then (x0, ((y boundMin) + dy / 2), z0, x1, ((y boundMin) + dy / 2), z1)
                                      else (x0, y0, ((z boundMin) + dz / 2), x1, y1, ((z boundMin) + dz / 2))
      midMin = T.point x2 y2 z2
      midMax = T.point x3 y3 z3
      left   = BoundingBox boundMin midMax
      right  = BoundingBox midMin boundMax
  in (left, right)

divide :: Shape -> Int -> Shape
divide s@Sphere {} t = s
divide s@Cube {} t = s
divide s@Cone {} t = s
divide s@Cylinder {} t = s
divide s@Triangle {} t = s
divide g@Group {} t =
  -- given a group, subdivide the group until threshold
  -- g' is a group with the children of the shapes that do not fit in
  --  the left/right partitions
  let g' = if t <= length (children g)
           then let (g', left, right) = partitionChildren g
                    -- subgroup (make a new group with the given children) and set
                    --  that group as the child to the input group
                    gLeftSub          = if null left
                                        then g'
                                        else makeSubgroup g' left
                    gRightSub         = if null right
                                        then gLeftSub
                                        else makeSubgroup gLeftSub right
                in gRightSub
           else g
  in  g' { children = map (\c -> divide c t) (children g') }

showGroupTree :: String -> Shape -> String
showGroupTree s g@Group {children = c} =
  " G(" ++ (show (length c)) ++ ")[" ++ (concatMap (\x -> (showGroupTree s x)) c) ++ "]"
showGroupTree s x  = s ++ " S[" ++ (show (Types.id x)) ++ "]"


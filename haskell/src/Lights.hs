module Lights
  ( Light (..)
  , pointLight
  , lighting
  )where

import Tuples
import Materials
import Patterns
import Shapes

data Light = Light { position  :: Tuple
                   , intensity :: Color}
             deriving (Show, Eq)

pointLight :: Tuple -> Color -> Light
pointLight = Light

lighting :: IsShape a => Material -> a -> Light -> Tuple -> Tuple -> Tuple -> Bool -> Color
lighting material object light point eyev normalv inShadow =
      -- combine the surface color with the light's color/intensity
  let materialColor  = case (materialPattern material) of
                         Nothing -> color material
                         Just p  -> patternAtShape p object point
      effectiveColor = materialColor `mulC` intensity light
      -- find the direction to the light source
      lightv         = norm (position light `sub` point)
      -- compute the ambient contribution
      ambientContrib = effectiveColor `mulCS` ambient material
      -- lightDotNormal represents the cosine of the angle between the
      -- light vector and the normal vector. A negative number means the
      -- light is on the other side of the surface
      lightDotNormal = lightv `dot` normalv
      (diffuseContrib, specularContrib) =
        if lightDotNormal < 0
        then (Color (Red 0) (Green 0) (Blue 0), Color (Red 0) (Green 0) (Blue 0))
        else
              -- compute the diffuse contribution
          let d = effectiveColor `mulCS` diffuse material `mulCS` lightDotNormal
              -- reflectDotEye represents the cosine of the angle between the
              -- reflection vector and the eye vector. A negative number means the
              -- light reflects away from the eye
              reflectv = reflect (neg lightv) normalv
              reflectDotEye = reflectv `dot` eyev
              spec = if reflectDotEye <= 0
                     then Color (Red 0) (Green 0) (Blue 0)
                     else let factor = reflectDotEye ** shininess material
                          in intensity light `mulCS` specular material `mulCS` factor
          in (d, spec)
  in if inShadow
     then ambientContrib
     else ambientContrib `addC` diffuseContrib `addC` specularContrib

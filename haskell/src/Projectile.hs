module Projectile where

import Tuples

{-
# projectile starts one unit above the origin.
# velocity is normalized to 1 unit/tick.
p ← projectile(point(0, 1, 0), normalize(vector(1, 1, 0)))

# gravity -0.1 unit/tick, and wind is -0.01 unit/tick.
e ← environment(vector(0, -0.1, 0), vector(-0.01, 0, 0))

function tick(env, proj)
  position ← proj.position + proj.velocity
  velocity ← proj.velocity + env.gravity + env.wind
  return projectile(position, velocity)
end function

-}

data Projectile = Projectile { position :: Tuple
                             , velocity :: Tuple }
                  deriving(Show)

data Environment = Environment { gravity :: Tuple
                               , wind    :: Tuple }
                   deriving(Show)

tick :: Environment -> Projectile -> Projectile
tick env proj = let pos = (position proj) `add` (velocity proj)
                    vel = (velocity proj) `add` (gravity env) `add` (wind env)
                in Projectile pos vel

launch' :: Environment -> Projectile -> [Projectile] -> [Projectile]
launch' env p@(Projectile (Tuple _ y _ _) _) xs
  | y < 0     = xs
  | otherwise = p : launch' env (tick env p) xs

launch :: Double -> Double -> [Projectile]
launch g w = let env  = Environment (vector 0 g 0) (vector w 0 0) 
                 proj = Projectile (point 0 1 0) (norm (vector 1 1 0))
             in launch' env proj []
                
-- REPL conveniance
testenv = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)
testorg = Projectile (point 0 1 0) (norm (vector 1 1 0))

result = tick testenv testorg

launchresult = launch (-0.5) (-0.01)
--

             



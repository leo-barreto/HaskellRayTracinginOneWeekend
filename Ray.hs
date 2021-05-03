module Ray where

import Vec3

data Ray = Ray {origin :: Vec3, direction :: Vec3} deriving Show

dirat :: Ray -> Double -> Vec3
dirat ray t = sumv (origin ray) (mulscalar (direction ray) (t)) 

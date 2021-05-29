module Hittable where

import Vec3
import Ray


data HitRecord = HitRecord { p :: Vec3
                           , normal :: Vec3
                           , t :: Double } deriving Show

notinrange :: Double -> Double -> Double -> Bool
notinrange root tmin tmax = (root < tmin || tmax < root)

setfacenormal :: Ray -> Vec3 -> Vec3
setfacenormal r outward
  | dot (direction r) outward > 0 = outward
  | otherwise = outward `mulscalar` (-1)

module Hittable where

import Vec3
import Ray

data HitRecord = HitRecord { p :: Vec3
                           , normal :: Vec3
                           , t :: Double } deriving Show

hit :: Ray -> Double -> Double -> HitRecord -> Bool
hit r tmin tmax hr = False

module Sphere where

import Vec3
import Ray
import Hittable

data Sphere = Sphere { center :: Vec3
                     , radius :: Double } deriving Show

hitsphere :: Sphere -> Ray -> Double -> Double -> HitRecord -> Bool
hitsphere s r tmin tmax hr
  | discriminant < 0 = False
  | (notinrange rootminus tmin tmax) && (notinrange rootplus tmin tmax) = False
  | otherwise = True
  where oc = (origin r) `subv` (center s)
        a = (direction r) `dot` (direction r)
        bh = oc `dot` (direction r)
        c = (oc `dot` oc) - (radius s) ** 2
        discriminant = bh ** 2 - a * c
        rootminus = (-bh - sqrt(discriminant)) / a
        rootplus = (-bh + sqrt(discriminant)) / a

notinrange :: Double -> Double -> Double -> Bool
notinrange root tmin tmax = (root < tmin || tmax < root)

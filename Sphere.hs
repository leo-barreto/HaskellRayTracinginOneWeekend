module Sphere where

import Vec3
import Ray
import Hittable

data Sphere = Sphere { center :: Vec3
                     , radius :: Double } deriving Show

hitsphere :: Sphere -> Ray -> Double -> Double -> HitRecord -> Bool
hitsphere s r tmin tmax hr
  | hr == hitrecsphere s r tmin tmax hr = False
  | otherwise = True

hitrecsphere :: Sphere -> Ray -> Double -> Double -> HitRecord -> HitRecord
hitrecsphere s r tmin tmax hr
  | discriminant < 0 = hr
  | (notinrange rootminus tmin tmax) && (notinrange rootplus tmin tmax) = hr
  | otherwise = newhr
  where oc = (origin r) `subv` (center s)
        a = (direction r) `dot` (direction r)
        bh = oc `dot` (direction r)
        c = (oc `dot` oc) - (radius s) ** 2
        discriminant = bh ** 2 - a * c
        rootminus = (-bh - sqrt(discriminant)) / a
        rootplus = (-bh + sqrt(discriminant)) / a
        
        -- Find correct root
        root
          | not (notinrange rootminus tmin tmax) = rootminus
          | otherwise = rootplus
        newp = dirat r root
        outward_normal = (newp `subv` (center s)) `divscalar` (radius s)
        newt = root
        newhr = HitRecord newp (setfacenormal r outward_normal) newt

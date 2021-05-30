module Hittable where

import Vec3
import Ray

data Hittable =
    Sphere { center :: Vec3, radius :: Double }
  | Empty
  deriving Show

data HitRecord = HitRecord { p :: Vec3
                           , normal :: Vec3
                           , t :: Double
                           } deriving (Show, Eq)


-- General Hittable
hit :: Hittable -> Ray -> Double -> Double -> HitRecord -> Bool
hit (Sphere center radius) r tmin tmax hr = hitsphere (Sphere center radius) r tmin tmax hr
hit _ r tmin tmax hr = False

hitrec :: Hittable -> Ray -> Double -> Double -> HitRecord -> HitRecord
hitrec (Sphere center radius) r tmin tmax hr = hitrecsphere (Sphere center radius) r tmin tmax hr
hitrec _ r tmin tmax hr = hr


-- Hittable list
anyhitrec :: [Hittable] -> Ray -> Double -> Double -> HitRecord -> HitRecord
anyhitrec [] r tmin tmax hr = hr
anyhitrec [a] r tmin tmax hr = hitrec a r tmin tmax hr
anyhitrec (a:as) r tmin tmax hr
  | hit a r tmin tmax hr == True = anyhitrec as r tmin newtmax newhr
  | otherwise = anyhitrec as r tmin tmax hr
  where newhr = hitrec a r tmin tmax hr
        newtmax = t newhr

anyhit :: [Hittable] -> Ray -> Double -> Double -> HitRecord -> Bool
anyhit la r tmin tmax hr
  | hr == anyhitrec la r tmin tmax hr = False
  | otherwise = True


-- Sphere
hitrecsphere :: Hittable -> Ray -> Double -> Double -> HitRecord -> HitRecord
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

hitsphere :: Hittable -> Ray -> Double -> Double -> HitRecord -> Bool
hitsphere s r tmin tmax hr
  | hr == hitrecsphere s r tmin tmax hr = False
  | otherwise = True


-- Auxiliary functions
notinrange :: Double -> Double -> Double -> Bool
notinrange root tmin tmax = (root < tmin || tmax < root)

setfacenormal :: Ray -> Vec3 -> Vec3
setfacenormal r outward
  | dot (direction r) outward > 0 = outward
  | otherwise = outward `mulscalar` (-1)

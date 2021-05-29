module HittableList where

import Vec3
import Ray
import Hittable
import Sphere

-- List of possible hittable objects
data PossibleHit = SphereVal Sphere

class Hittable a where
  hit :: a -> Ray -> Double -> Double -> Bool
  newhitrec :: a -> Ray -> Double -> Double -> HitRecord -> HitRecord

instance Hittable Sphere where
  hit = hitsphere
  newhitrec = hitrecsphere

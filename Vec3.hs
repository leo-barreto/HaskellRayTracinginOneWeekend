module Vec3 where

import System.Random


type Vec3 = (Double, Double, Double)

-- Vector utilities
norm :: Vec3 -> Double
norm (x, y, z) = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

coord :: Vec3 -> Int -> Double
coord (x, y, z) 0 = x
coord (x, y, z) 1 = y
coord (x, y, z) 2 = z
coord (x, y, z) _ = error "dimension not defined"

mulscalar :: Vec3 -> Double -> Vec3
mulscalar (x, y, z) i = (x * i, y * i, z * i)

divscalar :: Vec3 -> Double -> Vec3
divscalar (x, y, z) i = mulscalar (x, y, z) (1 / i)

sumv :: Vec3 -> Vec3 -> Vec3
sumv (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

subv :: Vec3 -> Vec3 -> Vec3
subv (x1, y1, z1) (x2, y2, z2) = sumv (x1, y1, z1) (mulscalar (x2, y2, z2) (-1))

dot :: Vec3 -> Vec3 -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2,
                                   z1 * x2 - x1 * z2,
                                   x1 * y2 - y1 * x2)

unitv :: Vec3 -> Vec3
unitv (x, y, z) = divscalar (x, y, z) (norm (x, y, z))

sumvectorarray :: [Vec3] -> Vec3
sumvectorarray [] = (0, 0, 0)
sumvectorarray (x:xs) = x `sumv`(sumvectorarray xs)

-- Random vector
randomvec :: (Double, Double) -> StdGen -> Vec3
randomvec (min, max) randgen = (x, y, z)
                               where [x, y, z] = take 3 (randomRs (min, max) randgen)

randomuvec :: StdGen -> Vec3
randomuvec randgen = unitv (randomvec (-1.0, 1.0) randgen)


-- Colour definition
type C3 = Vec3

writec :: C3 -> Double -> String
writec (r, g, b) samples = (show scaledr) ++ " " ++ (show scaledg) ++ " " ++ (show scaledb)
                           where scaledr = floor (256 * (clamp (r / samples) 0 0.999))
                                 scaledg = floor (256 * (clamp (g / samples) 0 0.999))
                                 scaledb = floor (256 * (clamp (b / samples) 0 0.999))

clamp :: Double -> Double -> Double -> Double
clamp x lower upper
  | x < lower = lower
  | x > upper = upper
  | otherwise = x


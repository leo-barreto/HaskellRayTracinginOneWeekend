module V3 where


type V3 = (Double, Double, Double)


norm :: V3 -> Double
norm (x, y, z) = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

coord :: V3 -> Int -> Double
coord (x, y, z) 0 = x
coord (x, y, z) 1 = y
coord (x, y, z) 2 = z
coord (x, y, z) _ = error "dimension not defined"

mulscalar :: V3 -> Double -> V3
mulscalar (x, y, z) i = (x * i, y * i, z * i)

divscalar :: V3 -> Double -> V3
divscalar (x, y, z) i = mulscalar (x, y, z) (1 / i)

sumv :: V3 -> V3 -> V3
sumv (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

subv :: V3 -> V3 -> V3
subv (x1, y1, z1) (x2, y2, z2) = sumv (x1, y1, z1) (mulscalar (x2, y2, z2) (-1))

dot :: V3 -> V3 -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: V3 -> V3 -> V3
cross (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2,
                                   z1 * x2 - x1 * z2,
                                   x1 * y2 - y1 * x2)

unitv :: V3 -> V3
unitv (x, y, z) = divscalar (x, y, z) (norm (x, y, z))

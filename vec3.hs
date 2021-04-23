module V3 where


data V3 = V3
          {x :: Double
          , y :: Double
          , z :: Double
          } deriving (Eq, Show)

class Vec3 a where
  norm :: a -> Double
  coord :: a -> Int -> Double
  mulc :: a -> Double -> V3
  divc :: a -> Double -> V3
  sumv :: a -> V3 -> V3
  subv :: a -> V3 -> V3
  dot :: a -> V3 -> Double
  cross :: a -> V3 -> V3
  unitv :: a -> V3

instance Vec3 V3 where
  norm (V3 x y z) = sqrt (x ^ 2 + y ^ 2 + z ^ 2)
  coord (V3 x y z) 0 = x
  coord (V3 x y z) 1 = y
  coord (V3 x y z) 2 = z
  coord (V3 x y z) _ = error "not enough dimensions"
  mulc (V3 x y z) i = V3 (x * i) (y * i) (z * i)
  divc (V3 x y z) i = mulc (V3 x y z) (1 / i)
  sumv (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)
  subv (V3 x1 y1 z1) (V3 x2 y2 z2) = sumv (V3 x1 y1 z1) (mulc (V3 x2 y2 z2) (-1))
  dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
  cross (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (y1 * z2 - z1 * y2)
                                         (z1 * x2 - x1 * z2)
                                         (x1 * y2 - y1 * x2)
  unitv (V3 x y z) = divc (V3 x y z) (norm (V3 x y z))

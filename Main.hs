import System.IO
import Vec3
import Ray


-- Variable definitions
-- Image
aspect_ratio = 16 / 9
image_width = 400
image_height = image_width / aspect_ratio
max_colour = 255

-- Camera
vp_height = 2
vp_width = aspect_ratio * vp_height
focal_len = 1
origin_cam = (0, 0, 0)
horizontal = (vp_width, 0, 0)
vertical = (0, vp_height, 0)
lower_left_corner = v1 `subv` v2 where v1 = origin_cam `subv` (divscalar horizontal 2)
                                       v2 = (divscalar vertical 2) `sumv` (0, 0, focal_len)


-- Auxiliary functions
gradient :: (Double, Double) -> String
gradient (x, y) = do
                  let v1 = lower_left_corner `sumv` (horizontal `mulscalar` x)
                      v2 = (vertical `mulscalar` y) `subv` origin_cam
                  writec (raycolour (Ray origin_cam (v1 `sumv` v2)))


ppmHeader file = do
                 writeFile file ("P3\n" ++ show (round image_width) ++ " " ++ show (round image_height))
                 appendFile file ("\n" ++ show (round max_colour) ++ "\n")


raycolour :: Ray -> C3
raycolour r
  | hs > 0 = ((coord n 0) + 1, (coord n 1) + 1, (coord n 2) + 1) `divscalar` 2
  | otherwise  = (mulscalar (1, 1, 1) (1 - t)) `sumv` (mulscalar (0.5, 0.7, 1.0) t)
  where n = unitv ((dirat r hs) `subv` (0, 0, -1))
        u = unitv (direction r)
        t = 0.5 * ((coord u 1) + 1)
        hs = hitsphere (0, 0, -1) 0.5 r


hitsphere :: Vec3 -> Double -> Ray -> Double
hitsphere center radius r
  | discriminant < 0 = -1
  | otherwise = (-bh - sqrt(discriminant)) / a
  where oc = (origin r) `subv` center
        a = (direction r) `dot` (direction r)
        bh = oc `dot` (direction r)
        c = (oc `dot` oc) - radius ** 2
        discriminant = bh ** 2 - a * c



main = do
       -- Render
       ppmHeader "test.ppm"
       let colour_gradx = [0..image_width - 1]
           colour_grady = [0..image_height - 1]
           elements = [(x / (image_width - 1), y / (image_height - 1)) | y <- reverse colour_grady, x <- colour_gradx]
       appendFile "test.ppm" (unlines (map gradient elements))

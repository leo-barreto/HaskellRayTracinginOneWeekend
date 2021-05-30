import System.IO
import Vec3
import Ray
import Hittable


-- Variable definitions
-- Math
infty = 1 / 0

-- Image
aspect_ratio = 16 / 9
image_width = 400
image_height = image_width / aspect_ratio
max_colour = 255
samples_per_pixel = 100

-- Camera
vp_height = 2
vp_width = aspect_ratio * vp_height
focal_len = 1
origin_cam = (0, 0, 0)
horizontal = (vp_width, 0, 0)
vertical = (0, vp_height, 0)
lower_left_corner = v1 `subv` v2 where v1 = origin_cam `subv` (divscalar horizontal 2)
                                       v2 = (divscalar vertical 2) `sumv` (0, 0, focal_len)

-- World
mainsphere = Sphere (0, 0, -1) 0.5
bgdsphere = Sphere (0, -100.5, -1) 100
world = [mainsphere, bgdsphere]

-- Auxiliary functions
gradient :: (Double, Double) -> String
gradient (x, y) = do
                  let v1 = lower_left_corner `sumv` (horizontal `mulscalar` x)
                      v2 = (vertical `mulscalar` y) `subv` origin_cam
                      hrbase = HitRecord (0, 0, 0) (0, 0, 0) 0
                  writec (raycolour world (Ray origin_cam (v1 `sumv` v2)) hrbase) samples_per_pixel


ppmHeader file = do
                 writeFile file ("P3\n" ++ show (round image_width) ++ " " ++ show (round image_height))
                 appendFile file ("\n" ++ show (round max_colour) ++ "\n")


raycolour :: [Hittable] -> Ray -> HitRecord -> C3
raycolour la r hr
  | anyhit la r 0 infty hr == True = ((normal newhr) `sumv` (1, 1, 1)) `divscalar` 2
  | otherwise  = (mulscalar (1, 1, 1) (1 - t)) `sumv` (mulscalar (0.5, 0.7, 1.0) t)
  where u = unitv (direction r)
        t = 0.5 * ((coord u 1) + 1)
        newhr = anyhitrec la r 0 infty hr


main = do
       -- Render
       ppmHeader "test.ppm"
       let colour_gradx = [0..image_width - 1]
           colour_grady = [0..image_height - 1]
           elements = [(x / (image_width - 1), y / (image_height - 1)) | y <- reverse colour_grady, x <- colour_gradx]
       appendFile "test.ppm" (unlines (map gradient elements))

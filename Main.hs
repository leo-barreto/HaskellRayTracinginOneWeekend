import System.IO
import Vec3
import Ray


raycolour :: Ray -> C3
raycolour r = do
              let u = unitv(direction r)
                  t = 0.5 * ((coord u 1) + 1)
              (mulscalar (1, 1, 1) (1 - t)) `sumv` (mulscalar (0.5, 0.7, 1.0) t)


-- Image
aspect_ratio = 16 / 9
image_width = 400
image_height = image_width / aspect_ratio
max_colour = 255

ppmHeader file = do
                 writeFile file ("\nP3\n" ++ show (round image_width) ++ " " ++ show (round image_height))
                 appendFile file ("\n" ++ show (round max_colour) ++ "\n")

-- Camera
vp_height = 2
vp_width = aspect_ratio * vp_height
focal_len = 1
origin_cam = (0, 0, 0)
horizontal = (vp_width, 0, 0)
vertical = (0, vp_height, 0)

lower_left_corner = v1 `subv` v2 where v1 = origin_cam `subv` (divscalar horizontal 2)
                                       v2 = (divscalar horizontal 2) `subv` (0, 0, focal_len)

gradient :: (Double, Double) -> String
gradient (x, y) = do
                  let v1 = origin_cam `sumv` (horizontal `mulscalar` x)
                      v2 = (vertical `mulscalar` y) `subv` origin_cam
                  writec (raycolour (Ray origin_cam (v1 `sumv` v2)))


main = do
       -- Render
       ppmHeader "test.ppm"
       let colour_gradx = [0..image_width - 1]
           colour_grady = [0..image_height - 1]
           elements = [(x, y) | y <- reverse colour_grady, x <- colour_gradx]
       appendFile "test.ppm" (unlines (map gradient elements))

import System.IO
import System.Random

import Vec3
import Ray
import Hittable
import Camera



-- Variable definitions
-- Math
infty = 1 / 0

-- Image
image_width = 400
image_height = image_width / aspect_ratio
max_colour = 255
samples_per_pixel = 20 :: Double

-- World
mainsphere = Sphere (0, 0, -1) 0.5
bgdsphere = Sphere (0, -100.5, -1) 100
world = [mainsphere, bgdsphere]

-- Auxiliary functions
gradient :: (Double, Double) -> StdGen -> String
gradient (x, y) randgen = do
                          let random2darray = randomarray (floor samples_per_pixel) randgen
                              v1 = lower_left_corner `sumv` (horizontal `mulscalar` x)
                              v2 = (vertical `mulscalar` y) `subv` origin_cam
                              hrbase = HitRecord (0, 0, 0) (0, 0, 0) 0
                               
                              -- Array of all shifted rays
                              mappedshiftedrays = map (\arg -> getshiftedray (x, y) arg) random2darray

                              -- Array of all colours
                              mappedcolours = map (\arg -> raycolour world arg hrbase) mappedshiftedrays

                          writec (sumvectorarray mappedcolours) samples_per_pixel


-- Generate random (i, j) list
randomarray :: Int -> StdGen -> [(Double, Double)]
randomarray samples randgen = take samples (randomRs ((0.0, 0.0), (1.0 / image_width, 1.0 / image_height)) randgen)


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


-- Main function 
main = do
       -- Render
       ppmHeader "test.ppm"

       -- Random seed
       randgen <- newStdGen

       -- Set pixels
       let colour_gradx = [0..image_width - 1]
           colour_grady = [0..image_height - 1]
           elements = [(x / (image_width - 1), y / (image_height - 1)) | y <- reverse colour_grady, x <- colour_gradx]
           
           -- Partial application with lambda
           mappedgrad = map (\arg -> gradient arg randgen) elements

       appendFile "test.ppm" (unlines mappedgrad)
       putStrLn "ppm file created!"


import System.IO
import System.Random
import Control.Monad

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
max_depth = 50


-- World
mainsphere = Sphere (0, 0, -1) 0.5
bgdsphere = Sphere (0, -100.5, -1) 100
world = [mainsphere, bgdsphere]


-- Auxiliary functions
gradient :: (Double, Double) -> IO String
gradient (x, y) = do
                 
                  -- List of random arrays for diffuse material
                  randomunitvs <- replicateM max_depth randomuvec
                  
                  -- Seed for anti-aliasing (aa)
                  randgen <- newStdGen
 
                  let aa_array = randomarray (floor samples_per_pixel) randgen
                      hrbase = HitRecord (0, 0, 0) (0, 0, 0) 0
                               
                      -- Array of all shifted rays
                      mappedshiftedrays = map (\arg -> getshiftedray (x, y) arg) aa_array

                      -- Array of all colours
                      mappedcolours = map (\arg -> raycolour world arg hrbase randomunitvs max_depth) mappedshiftedrays

                  writec (sumvectorarray mappedcolours) samples_per_pixel


-- Generate random (i, j) list
randomarray :: Int -> StdGen -> [(Double, Double)]
randomarray samples randgen = take samples (randomRs ((0.0, 0.0), (1.0 / image_width, 1.0 / image_height)) randgen)


raycolour :: [Hittable] -> Ray -> HitRecord -> [Vec3] -> Int -> C3
raycolour la r hr rvecs depth
  | depth <= 0 = (0, 0, 0)
  | anyhit la r 0.001 infty hr == True = (raycolour la newray hr rvecs (depth - 1)) `divscalar` 2
  | otherwise  = (mulscalar (1, 1, 1) (1 - t)) `sumv` (mulscalar (0.5, 0.7, 1.0) t)
  where newhr = anyhitrec la r 0 infty hr
        rvec = rvecs !! (depth - 1)
        target = ((normal newhr) `sumv` (p newhr)) `sumv` rvec
        newray = Ray (p newhr) (target `subv` (p newhr))
        u = unitv (direction r)
        t = 0.5 * ((coord u 1) + 1)


mappedgrad :: [(Double, Double)] -> IO [String]
mappedgrad array = sequenceA (map (\arg -> gradient arg) array)


ppmHeader file = do
                 writeFile file ("P3\n" ++ show (round image_width) ++ " " ++ show (round image_height))
                 appendFile file ("\n" ++ show (round max_colour) ++ "\n")

-- Main function 
main = do
       -- Render
       ppmHeader "test.ppm"

       -- Set pixels
       let colour_gradx = [0..image_width - 1]
           colour_grady = [0..image_height - 1]
           elements = [(x / (image_width - 1), y / (image_height - 1)) | y <- reverse colour_grady, x <- colour_gradx]
       
       -- A little bit of dark magic    
       mgrad <- mappedgrad elements

       appendFile "test.ppm" (unlines mgrad)
       putStrLn "ppm file created!"


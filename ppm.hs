import System.IO
import Vec3

width = 256
height = 256
max_colour = 255 :: Int


ppmHeader file = do
                 writeFile file "# This file was created with the ppmHeader function"
                 appendFile file ("\nP3\n" ++ show width ++ " " ++ show height)
                 appendFile file ("\n" ++ show max_colour ++ "\n")

ppmFirst file = do
                let colour_grad = [0..max_colour]
                    elements = [(fromIntegral y, fromIntegral x, fromIntegral max_colour * 0.25) | x <- reverse colour_grad, y <- colour_grad]
                ppmRows <- mapM (\colour -> writec colour 1) elements
                appendFile file (unlines ppmRows)
main = do
       ppmHeader "test.ppm"
       ppmFirst "test.ppm"
       putStrLn "Done!"

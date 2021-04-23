import System.IO
import Vec3

width = 256
height = 256
max_colour = 255


ppmHeader file = do
                 writeFile file "# This file was created with the ppmHeader function"
                 appendFile file ("\nP3\n" ++ show width ++ " " ++ show height)
                 appendFile file ("\n" ++ show (round max_colour) ++ "\n")

ppmFirst file = do
                let colour_grad = [0..max_colour]
                    elements = [(y, x, max_colour * 0.25) | x <- reverse colour_grad, y <- colour_grad]
                appendFile file (unlines (map writec elements))
main = do
       ppmHeader "test.ppm"
       ppmFirst "test.ppm"
       putStrLn "Done!"

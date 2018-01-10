module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Hetris" (1024, 768) (200, 200)

background :: Color
background = black

main :: IO ()
main = display window black (circle 10)

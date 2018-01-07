module Main where

import Graphics.Gloss


window :: Display
window = InWindow "NiceFucker" (200, 200) (10, 10)

background ::Color
background  = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing

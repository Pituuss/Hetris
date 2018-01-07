module Main where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "NiceFucker" (width, height) (offset, offset)

background ::Color
background  = black

main :: IO ()
main = display window background drawing

drawing :: Picture
drawing = pictures
  [
  color ballColor $ circleSolid 20
  ,color paddleColor $ rectangleSolid 10 50
  ]
  where
    ballColor = dark red
    paddleColor = light (light blue)

module Main where

import BoardRenderer
import Graphics.Gloss
import State

window :: Display
window = InWindow "Hetris" (1024, 768) (0, 0)

background :: Color
background = black

state = initialGameState

main :: IO ()
main =
  display
    window
    white
    (pictures
       [ renderWall
       , uncurry translate (toScreenCoords (blockPos state)) $
         color cyan $ rectangleSolid (0.9 * 32) (0.9 * 32)
       ])

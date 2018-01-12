module Main where

import BoardRenderer
import GameLogic
import Graphics.Gloss
import State

window :: Display
window = InWindow "Hetris" (1024, 768) (0, 0)

background :: Color
background = black

-- state = initialGameState
main :: IO ()
main = simulate window white 5 initialGameState renderTMPFoo simpleFalling

renderTMPFoo :: State -> Picture
renderTMPFoo state =
  pictures
    [ renderWall
    , uncurry translate (toScreenCoords (blockPos state)) $
      color cyan $ rectangleSolid (0.9 * 32) (0.9 * 32)
    ]

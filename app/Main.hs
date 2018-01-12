module Main where

import Blocks
import BoardRenderer
import GameBoard
import GameLogic
import Graphics.Gloss
import State

window :: Display
window = InWindow "Hetris" (1024, 768) (0, 0)

background :: Color
background = black

-- state = initialGameState
main :: IO ()
main = simulate window white 1 initialGameState render simpleFalling

renderTMPFoo :: State -> Picture
renderTMPFoo state =
  pictures [renderBoard $ renderBlock getBlock (0, 0) emptyBoard]
 -- = renderBoard $ renderBlock (block state) (blockPos state) (gameBoard state)
 -- pictures [renderBoard $ gameBoard state]
 -- renderBoard $ renderBlock (block state) (blockPos state) emptyBoard
 -- =
 --  pictures
 --    [ renderWall
 --    , uncurry translate (toScreenCoords (blockPos state)) $
 --      color cyan $ rectangleSolid (0.9 * 32) (0.9 * 32)
 -- ]

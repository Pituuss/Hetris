module BoardRenderer
  ( renderWall
  , toScreenCoords
  ) where

import Blocks
import GameBoard
import Graphics.Gloss
import State

cellSize = 32

wallColor = dark red

boardWidth = 11 * cellSize

boardHeight = 21 * cellSize

padding = 70

boardColor = black

toScreenCoords :: (Float, Float) -> (Float, Float)
toScreenCoords (x1, y1) = (x2, y2)
  where
    x2 = x1 * cellSize - 5 * cellSize
    y2 = 10 * cellSize - (y1 * cellSize)

renderWall :: Picture
renderWall =
  pictures
    [ color wallColor $
      rectangleSolid (boardWidth + padding) (boardHeight + padding)
    , color boardColor $ rectangleSolid boardWidth boardHeight
    ]
-- renderBoard ::Board -> Picture
-- renderBoard board = pictures (map cellToPic (cellCoord board))

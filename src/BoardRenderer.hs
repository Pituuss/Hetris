module BoardRenderer
  ( renderWall
  , toScreenCoords
  , render
  , renderBoard
  , renderBlock
  , renderCell
  , cellsCoords
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
    y2 = 11 * cellSize - (y1 * cellSize)

renderWall :: Picture
renderWall =
  pictures
    [ color wallColor $
      rectangleSolid (boardWidth + padding) (boardHeight + padding)
    , color boardColor $ rectangleSolid boardWidth boardHeight
    ]

cellsCoords :: Board -> [(Float, Float, Cell)]
cellsCoords board = concatMap extractRows (numberRows board)
  where
    extractRows :: (Float, Row) -> [(Float, Float, Cell)]
    extractRows (y, row) = map extractCells (numberCells row)
      where
        extractCells (x, c) = (x, y, c)

renderCell :: (Float, Float) -> Color -> Picture
renderCell (x, y) col = translate x' y' $ color col $ rectangleSolid size' size'
  where
    x' = fst $ toScreenCoords (x, y)
    y' = snd $ toScreenCoords (x, y)
    size' = cellSize * 0.8

renderBoard :: Board -> Picture
renderBoard board = pictures (map cellToPic (cellsCoords board))
  where
    cellToPic (x, y, cell)
      | y < 1 = pictures []
      | cell == Empty = pictures []
      | otherwise = renderCell (x, y) (cellColor cell)

renderBlock :: Block -> (Float, Float) -> Board -> Board
renderBlock block (x, y) board = BoardOfRows $ map renderRow $ numberRows board
  where
    renderRow (yP, row) = RowOfCells $ map renderCellsInRows (numberCells row)
      where
        renderCellsInRows (xP, cell)
          | cell /= Empty = cell
          | blockHasCoord (x - xP, y - yP) block = FilledWith (blockColor block)
          | otherwise = Empty

render :: State -> Picture
render state = pictures [walls, currentBoard, activeBlock]
  where
    walls = renderWall
    currentBoard = pictures [renderBoard $ gameBoard state]
    activeBlock =
      renderBoard $ renderBlock (block state) (blockPos state) (gameBoard state)

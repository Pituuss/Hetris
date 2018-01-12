module GameBoard
  ( cellColor
  , Board(..)
  , Cell(..)
  , emptyRow
  , emptyBoard
  ) where

import Blocks
import Graphics.Gloss

data Cell
  = Empty
  | FilledWith Color
  deriving (Show, Eq)

data Row =
  RowOfCells [Cell]
  deriving (Show)

data Board =
  BoardOfRows [Row]
  deriving (Show)

numberRows :: Board -> [(Int, Row)]
numberRows (BoardOfRows rows) = zip [1 .. 22] rows

numberCells :: Row -> [(Int, Cell)]
numberCells (RowOfCells cells) = zip [1 .. 10] cells

cell Empty = black
cell (FilledWith color) = color

cellColor :: Cell -> Color
cellColor Empty = black
cellColor (FilledWith color) = color

emptyRow :: Row
emptyRow = RowOfCells (replicate 10 Empty)

emptyBoard :: Board
emptyBoard = BoardOfRows (replicate 22 (RowOfCells []))
-- drawBlock :: Block -> (Int, Int) -> Board -> Board
-- drawBlock

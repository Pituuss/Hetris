module GameBoard
  ( cellColor -- :: Cell -> Color
  ) where

import Block
import Graphics.Gloss

data Cell
  = Empty
  | FilledWith Color
  deriving (Show, Eq)

cellColor :: Cell -> Color
cell Empty = black
cell (FilledWith color) = color

data Row =
  RowOfCells [Cell]
  deriving (Show)

empytRow :: Row
emptyRow = RowOfCells (replicate 10 Empty)

data Board =
  BoardOfRows [Row]
  deriving (Show)

emptyBoard :: Board
emptyBoard = BoardOfRows (replicate 22)

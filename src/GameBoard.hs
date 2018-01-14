module GameBoard
  ( cellColor
  , numberRows
  , numberCells
  , Board(..)
  , Row(..)
  , Cell(..)
  , emptyRow
  , emptyBoard
  , rowsToBoard
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

numberRows :: Board -> [(Float, Row)]
numberRows (BoardOfRows rows) = zip [0 .. 21] rows

numberCells :: Row -> [(Float, Cell)]
numberCells (RowOfCells cells) = zip [0 .. 9] cells

rowsToBoard :: [(Float,Row)] -> Board
rowsToBoard x = BoardOfRows (unZip x)

unZip :: [(Float,Row)] -> [Row]
unZip [] = []
unZip (x:xs) = [snd x] ++ (unZip xs)  

cell Empty = black
cell (FilledWith color) = color

cellColor :: Cell -> Color
cellColor Empty = black
cellColor (FilledWith color) = color

emptyRow :: Row
emptyRow = RowOfCells (replicate 10 Empty)

emptyBoard :: Board
emptyBoard = BoardOfRows (replicate 22 emptyRow)

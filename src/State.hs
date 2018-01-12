module State
  ( initialGameState
  , State(..)
  ) where

import Blocks
import GameBoard

data State = State
  { score :: Integer
  , gameBoard :: Board
  , block :: Block
  , blockPos :: (Float, Float)
  , accelerate :: Bool
  } deriving (Show)

initialGameState =
  State
  { score = 0
  , gameBoard = emptyBoard
  , block = getBlock
  , blockPos = (4, 0)
  , accelerate = True
  }

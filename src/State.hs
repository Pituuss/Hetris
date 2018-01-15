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
  , randSeed :: Float
  } deriving (Show)

initialGameState =
  State
  { score = 0
  , gameBoard = emptyBoard
  , blockPos = (5, 0)
  , accelerate = True
  , block = getBlock
  , randSeed = 0
  }

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
  , blockNumber :: Int
  } deriving (Show)

initialGameState =
  State
  { score = 0
  , gameBoard = emptyBoard
  , blockPos = (9, 0)
  , accelerate = True
  , blockNumber = 0
  , block = getBlock
  } 

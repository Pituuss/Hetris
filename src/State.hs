module State
  ( initialGameState
  , State(..)
  ) where

import Blocks
import GameBoard

data State = State
  { score :: Integer
  , time :: Float
  , dTime :: Float
  , timeToNextMove :: Float
  , gameBoard :: Board
  , block :: Block
  , blockPos :: (Float, Float)
  , accelerate :: Bool
  , randSeed :: Float
  } deriving (Show)

initialGameState =
  State
  { score = 0
  , time = 0
  , dTime = 0
  , timeToNextMove = 0
  , gameBoard = emptyBoard
  , blockPos = (5, 0)
  , accelerate = True
  , block = newBlock $ randSeed initialGameState
  , randSeed = 0
  }

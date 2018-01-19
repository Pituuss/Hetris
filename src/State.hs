module State
  ( initialGameState
  , State(..)
  ) where

import Blocks
import GameBoard
import System.Random

data State = State
  { score :: Integer
  , time :: Float
  , dTime :: Float
  , timeToNextMove :: Float
  , gameBoard :: Board
  , block :: Block
  , blockPos :: (Float, Float)
  , randSeed :: StdGen
  } deriving (Show)

initialGameState =
  State
  { score = 0
  , time = 0
  , dTime = 0
  , timeToNextMove = 0
  , gameBoard = emptyBoard
  , blockPos = (5, 0)
  , block = newBlock 1
  , randSeed = mkStdGen 0
  }

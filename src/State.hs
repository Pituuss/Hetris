{-|
Module : State
|-}
module State
  ( initialGameState
  , State(..)
  ) where

import           Blocks
import           GameBoard
import           System.Random

-- | Data describing current game state
data State = State
  { score          :: Integer -- ^ Current score
  , time           :: Float -- ^ Current time
  , dTime          :: Float -- ^ dTime passed
  , timeToNextMove :: Float -- ^ current time to the next move
  , gameBoard      :: Board -- ^ current board with cells
  , block          :: Block -- ^ current block we use
  , blockPos       :: (Float, Float) -- ^ current blocks position
  , randSeed       :: StdGen -- ^ random seed
  , change         :: Bool
  } deriving (Show)

-- | initial state of the game
initialGameState :: State
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
  , change = True
  }

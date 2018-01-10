module State
  ( initialGameState
  , resetGameState
  ) where

import GameBaord

data State = State
  { score :: Integer
  , gameBoard :: Board
  , block :: Block
  , blockPos :: Block
  , accelerate :: Bool
  } deriving (Show)

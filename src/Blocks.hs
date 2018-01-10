module Blocks
  ( Block
  ) where

import Graphics.Gloss

data Block = BlockCoords
  { x :: Float
  , y :: Float
  , color :: Color
  } deriving (Show)

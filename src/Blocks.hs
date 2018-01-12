module Blocks
  ( Block(..)
  , getBlock
  ) where

import Graphics.Gloss

data Block =
  BlockCoords [(Int, Int)]
              Color
  deriving (Show)

getBlock = BlockCoords [(0, 0)] (light blue)

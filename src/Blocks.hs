module Blocks
  ( Block(..)
  , getBlock
  ) where

import Graphics.Gloss

data Block =
  BlockCoords [(Float, Float)]
              Color
  deriving (Show)

getBlock = BlockCoords [(0, 0)] (light blue)

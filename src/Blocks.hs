module Blocks
  ( Block(..)
  , blockHasCoord
  , blockColor
  , getBlock
  ) where

import Graphics.Gloss

data Block =
  BlockCoords [(Float, Float)]
              Color
  deriving (Show)

getBlock :: Block
getBlock = BlockCoords [(0, 0)] (light blue)

blockColor :: Block -> Color
blockColor (BlockCoords _ color) = color

blockHasCoord :: (Float, Float) -> Block -> Bool
blockHasCoord coords (BlockCoords coords' _) = coords `elem` coords'

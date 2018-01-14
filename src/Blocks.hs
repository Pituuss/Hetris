module Blocks
  ( Block(..)
  , blockHasCoord
  , blockColor
  , getBlock
  , blockList
  , randomNewBlock
  ) where

import Graphics.Gloss

data Block =
  BlockCoords [(Float, Float)]
              Color
  deriving (Show)

getBlock :: Block
getBlock = BlockCoords [(0, 0), (0, 1), (0, 2),(-1,1)] (light blue)


blockColor :: Block -> Color
blockColor (BlockCoords _ color) = color

blockHasCoord :: (Float, Float) -> Block -> Bool
blockHasCoord coords (BlockCoords coords' _) = coords `elem` coords'

blockList :: Block -> [(Float, Float)]
blockList (BlockCoords x _) = x

randomNewBlock :: Block
randomNewBlock = randomNewBlockx 1 --tu maja sie jakos literki zmieniac albo losowac

randomNewBlockx :: Int -> Block
randomNewBlockx number =
  case number of
    1 -> BlockCoords [(0, 0), (0, 1), (0, 2), (2, 2)] (light blue)
    2 -> BlockCoords [(0, 0), (0, 1), (0, 2)] (light blue)
    3 -> BlockCoords [(0, 0), (0, 1)] (light blue)

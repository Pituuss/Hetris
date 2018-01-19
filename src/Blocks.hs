module Blocks
  ( Block(..)
  , blockHasCoord
  , blockColor
  , rotateBlockCW
  , rotateBlockCCW
  , newBlock
  , blockCords
  ) where

import Graphics.Gloss

data Block =
  BlockCoords [(Float, Float)]
              Color
  deriving (Show)

blockColor :: Block -> Color
blockColor (BlockCoords _ color) = color

blockCords :: Block -> [(Float, Float)]
blockCords (BlockCoords cor _) = cor

blockHasCoord :: (Float, Float) -> Block -> Bool
blockHasCoord coords (BlockCoords coords' _) = coords `elem` coords'

newBlock :: Double -> Block
newBlock r =
  case (truncate (r * 1000)) `mod` 6 of
    0 -> BlockCoords [(0, 0), (0, 1), (-1, 0), (-1, 1)] (light blue)
    1 -> BlockCoords [(0, 0), (1, 0), (0, 1), (0, -1)] (light yellow)
    2 -> BlockCoords [(0, 0), (1, 0), (2, 0), (-1, 0)] (light orange)
    3 -> BlockCoords [(0, 0), (1, 0), (0, -1), (-1, -1)] (light green)
    4 -> BlockCoords [(0, 0), (0, 1), (1, 0), (-1, 0)] (light red)
    5 -> BlockCoords [(0, 0), (0, -1), (0, -1), (1, -1)] (light cyan)

-- co to robi ??
myMod :: Float -> Float -> Float
myMod x y =
  if x < y
    then x
    else myMod (x - y) y

rotateBlockCW :: Block -> Block
rotateBlockCW (BlockCoords cords col) = BlockCoords (map rotateCW cords) col
  where
    rotateCW (a, b) = (-b, a)

rotateBlockCCW :: Block -> Block
rotateBlockCCW (BlockCoords cords col) = BlockCoords (map rotateCW cords) col
  where
    rotateCW (a, b) = (b, -a)

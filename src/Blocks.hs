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

newBlock :: Float -> Block
newBlock number
  | numb == 0 = BlockCoords [(0, 0), (0, 1), (1, 0), (1, 1)] (light blue)
  | numb == 1 = BlockCoords [(0, 0), (0, 1), (1, 1), (1, 2)] (light yellow)
  | numb == 2 = BlockCoords [(0, 0), (1, 0), (2, 0), (-1, 0)] (light orange)
  | numb == 3 = BlockCoords [(0, 0), (0, 1), (0, 2), (1, 2)] (light green)
  | numb == 4 = BlockCoords [(0, 0), (0, 1), (0, 2), (1, 1)] (light red)
  | otherwise = BlockCoords [(0, 0), (0, 1), (0, 2), (-1, 1)] (light cyan)
  where
    numb = myMod number 6

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

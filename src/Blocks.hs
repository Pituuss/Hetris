{-|
Module : Blocks
|-}
module Blocks
  ( Block(..)
  , blockHasCoord
  , blockColor
  , rotateBlockCW
  , rotateBlockCCW
  , newBlock
  , blockCords
  ) where

import           Graphics.Gloss

-- | data representing abstract 2D block
data Block =
  BlockCoords [(Float, Float)] -- ^ blocks coordinates
              Color --  ^ blocks color
  deriving (Show)

-- | function returning blocks color
blockColor :: Block -> Color
blockColor (BlockCoords _ color) = color

-- | function returning blocks coordinates
blockCords :: Block -> [(Float, Float)]
blockCords (BlockCoords cor _) = cor

-- | checking if given pair (a,b) is in blocks coordinates
blockHasCoord :: (Float, Float) -> Block -> Bool
blockHasCoord coords (BlockCoords coords' _) = coords `elem` coords'

-- | getting new random block
newBlock :: Double -> Block
newBlock r =
  case truncate (r * 1000) `mod` 6 of
    0 -> BlockCoords [(0, 0), (0, 1), (-1, 0), (-1, 1)] (light blue)
    1 -> BlockCoords [(0, 0), (1, 0), (0, 1), (0, -1)] (light yellow)
    2 -> BlockCoords [(0, 0), (1, 0), (2, 0), (-1, 0)] orange
    3 -> BlockCoords [(0, 0), (1, 0), (0, -1), (-1, -1)] (light green)
    4 -> BlockCoords [(0, 0), (0, 1), (1, 0), (-1, 0)] (light red)
    5 -> BlockCoords [(0, 0), (0, -1), (-1, 0), (1, -1)] (light cyan)

-- | clockwise block rotation
rotateBlockCW :: Block -> Block
rotateBlockCW (BlockCoords cords col) = BlockCoords (map rotateCW cords) col
  where
    rotateCW (a, b) = (-b, a)

-- | counter clockwise block rotation
rotateBlockCCW :: Block -> Block
rotateBlockCCW (BlockCoords cords col) = BlockCoords (map rotateCW cords) col
  where
    rotateCW (a, b) = (b, -a)

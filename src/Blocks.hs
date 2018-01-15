module Blocks
  ( Block(..)
  , blockHasCoord
  , blockColor
  , getBlock
  , blockList
  , newBlock
  , rotateBlock
  ) where

import Graphics.Gloss

data Block =
  BlockCoords [(Float, Float)]
              Color
  deriving (Show)

getBlock :: Block
getBlock = BlockCoords [(0, 0),(1,0)] (light blue)

blockColor :: Block -> Color
blockColor (BlockCoords _ color) = color

blockHasCoord :: (Float, Float) -> Block -> Bool
blockHasCoord coords (BlockCoords coords' _) = coords `elem` coords'

blockList :: Block -> [(Float, Float)]
blockList (BlockCoords x _) = x

newBlock :: Float -> Block
newBlock number = 
  if numb == 0 
    then BlockCoords [(0, 0), (0, 1), (1, 0), (1, 1)] (light blue)
  else if numb == 1
    then BlockCoords [(0, 0), (0, 1), (1, 1),(1,2)] (light blue)
  else if numb == 2
    then BlockCoords [(0, 0), (1, 0), (2, 0),(3,0)] (light blue)
  else if numb == 3
    then BlockCoords [(0, 0), (0, 1), (0, 2),(1,2)] (light blue)
  else if numb == 4
    then BlockCoords [(0, 0), (0, 1), (0, 2),(1,1)] (light blue)
  else if numb == 5
    then getBlock
  else 
    BlockCoords [(0, 0), (0, 1), (0, 2),(-1,1)] (light blue)
    where 
      numb = myMod number 6

myMod :: Float -> Float -> Float
myMod x y = if x < y then x else myMod (x-y) y

rotateBlock :: [(Float,Float)] -> Block
rotateBlock x = BlockCoords (rotateB x (head x)) (light blue)

rotateB :: [(Float,Float)] -> (Float,Float) -> [(Float,Float)]
rotateB [] _ = []
rotateB (x:xs) first = if x == first then [x] ++ rotateB xs first
  else [(-(snd x),fst x)] ++ rotateB xs first
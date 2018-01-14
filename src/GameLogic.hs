module GameLogic
  ( simpleFalling
  ) where

import Blocks
import BoardRenderer
import GameBoard
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import State

simpleFalling :: Float -> State -> State --ViewPort ->
simpleFalling = moveBlock

moveBlock :: Float -> State -> State
moveBlock seconds state =
  if isNotColision state {blockPos = (x, y - 1)}
    then state {blockPos = (x, y')}
    else loadNewState state
  where
    (x, y) = blockPos state
    y' = y + 1

isNotColision :: State -> Bool
isNotColision state =
  if (\(a, b) -> b) (blockPos state) < 0
    then True
    else if bottomWallColision (blockCoordList state) ||
            mapColision state (blockCoordList state)
           then False
           else True

bottomWallColision :: [(Float, Float)] -> Bool
bottomWallColision [] = False
bottomWallColision (x:xs) = result || bottomWallColision xs
  where
    result =
      if (\(a, b) -> b) x > 22
        then True
        else False

loadNewState :: State -> State
loadNewState state =
  state
  { blockPos = (7, 0)
  , gameBoard = renderBlock (block state) (blockPos state) (gameBoard state)
  }

mapColision :: State -> [(Float, Float)] -> Bool
mapColision state [] = False
mapColision state x =
  pointColision (head x) (numberRows (gameBoard state)) ||
  mapColision state (tail x)

pointColision :: (Float, Float) -> [(Float, Row)] -> Bool
pointColision (pointx, pointy) x =
  if (\(a, b) -> a) (head x) == pointy
    then colision pointx (numberCells ((\(a, b) -> b) (head x)))
    else pointColision (pointx, pointy) (tail x)

colision :: Float -> [(Float, Cell)] -> Bool
colision yx x =
  if yx == (\(a, b) -> a) (head x)
    then check
    else colision yx (tail x)
  where
    check =
      if cellColor ((\(a, b) -> b) (head x)) == black
        then False
        else True

blockCoordList :: State -> [(Float, Float)]
blockCoordList state = listConvert (blockList (block state)) state

listConvert :: [(Float, Float)] -> State -> [(Float, Float)]
listConvert [] _ = []
listConvert x state =
  [convert (head x) (blockPos state)] ++ listConvert (tail x) state

convert :: (Float, Float) -> (Float, Float) -> (Float, Float)
convert (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

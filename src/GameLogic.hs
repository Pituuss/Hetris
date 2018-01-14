module GameLogic
  ( simpleFalling
    ,blockCoordList
    ,mapColision
  ) where

import Blocks
import BoardRenderer
import GameBoard
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import State

simpleFalling :: Float -> State -> State 
simpleFalling = moveBlock

removeFullRows :: State -> State
removeFullRows state = state --checkRows (numberRows (gameBoard state))
 
--checkRows :: [(Float, Row)] -> Bool

moveBlock :: Float -> State -> State
moveBlock seconds state =
  if isNotColision state {blockPos = (x, y + 1)}
    then  state {blockPos = (x, y')}
    else  removeFullRows (loadNewState state)
  where
    (x, y) = blockPos state
    y' = y + 1

isNotColision :: State -> Bool
isNotColision state =
  if snd (blockPos state) < 0
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
      if snd x > 13                   --zmienic na 22
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
pointColision _ [] = False
pointColision (pointx, pointy) x =
  if fst (head x) == pointy
    then colision pointx (numberCells (snd (head x)))
    else pointColision (pointx, pointy) (tail x)

colision :: Float -> [(Float, Cell)] -> Bool
colision _ [] = False
colision yx x =
  if yx == fst (head x)
    then check
    else colision yx (tail x)
  where
    check =
      if cellColor (snd (head x)) == black
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

module GameLogic
  ( updateGameState
  , blockCoordList
  , mapColision
  , isNotColision
  ) where

import Blocks
import BoardRenderer
import GameBoard
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import State
import System.Random

blockVel :: Float
blockVel = 5

accBlockVel :: Float
accBlockVel = 30

blockMovTime :: State -> Float
blockMovTime state = 1.0 / blockVel

updateGameState :: Float -> State -> State
updateGameState tm state = updater state {time = time state + tm, dTime = tm}

updater :: State -> State
updater state
  | timeToNextMove (updateClock state) <= 0 =
    moveBlock (updateClock state) {timeToNextMove = blockMovTime state}
  | otherwise = state {timeToNextMove = timeToNextMove state - dTime state}
  where
    updateClock state =
      state {timeToNextMove = timeToNextMove state - dTime state}

removeFullRows :: State -> State
removeFullRows state = checkBoard (numberRows (gameBoard state)) state

checkBoard :: [(Float, Row)] -> State -> State
checkBoard [] state = state
checkBoard (x:xs) state =
  if isRowFull (numberCells (snd x))
    then removeFullRows
           (state
            { score = score'
            , gameBoard =
                rowsToBoard
                  (removeRow (numberRows (gameBoard state)) emptyRow (fst x)) --od ilu ma zmieni
            })
    else checkBoard xs state
  where
    score' = score state + 1

removeRow :: [(Float, Row)] -> Row -> Float -> [(Float, Row)]
removeRow [] _ _ = []
removeRow (x:xs) lastRow fullrow
  | fst x > fullrow = x : xs
  | fst x == 0 = (0, emptyRow) : removeRow xs (snd x) fullrow
  | otherwise = (fst x, lastRow) : removeRow xs (snd x) fullrow

isRowFull :: [(Float, Cell)] -> Bool
isRowFull [] = True
isRowFull (x:xs) = (cellColor (snd x) /= black) && isRowFull xs

moveBlock :: State -> State
moveBlock state =
  if isNotColision state {blockPos = (x, y + 1)}
    then state {blockPos = (x, y')}
    else removeFullRows $ loadNewState state
  where
    (x, y) = blockPos state
    y' = y + 1

isNotColision :: State -> Bool
isNotColision state
  | snd (blockPos state) < 0 = True
  | bottomWallColision (blockCoordList state) ||
      mapColision state (blockCoordList state) = False
  | otherwise = True

bottomWallColision :: [(Float, Float)] -> Bool
bottomWallColision [] = False
bottomWallColision (x:xs) = result || bottomWallColision xs
  where
    result = snd x > 21

loadNewState :: State -> State
loadNewState state =
  state
  { blockPos = (4, 0)
  , gameBoard = renderBlock (block state) (blockPos state) (gameBoard state)
  , block = newBlock $ fst newSeed
  , randSeed = snd newSeed
  }
  where
    newSeed = randomR (0.0, 1.0) (randSeed state)

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
    then cellColor (snd (head x)) /= black
    else colision yx (tail x)

-- randomNewBlock :: State -> State
-- randomNewBlock state = state {block = newBlock $ randSeed state}
blockCoordList :: State -> [(Float, Float)]
blockCoordList state = listConvert (blockCords $ block state) state

listConvert :: [(Float, Float)] -> State -> [(Float, Float)]
listConvert [] _ = []
listConvert x state =
  convert (head x) (blockPos state) : listConvert (tail x) state

convert :: (Float, Float) -> (Float, Float) -> (Float, Float)
convert (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

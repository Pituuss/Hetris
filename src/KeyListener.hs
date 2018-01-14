module KeyListener
  ( handleKeys
  ) where

import Graphics.Gloss.Interface.Pure.Game
import State
import GameLogic
import Blocks

handleKeys :: Event -> State -> State
handleKeys (EventKey (Char 'a') pos _ _) state =
  if canUpdate (state {blockPos = (x - 1, y)}) && pos == Down
    then state {blockPos = (x', y)}
    else state
  where
    (x, y) = blockPos state
    x' = x - 1
handleKeys (EventKey (Char 'd') pos _ _) state =
  if canUpdate (state {blockPos = (x + 1, y)}) && pos == Down
    then state {blockPos = (x', y)}
    else state
  where
    (x, y) = blockPos state
    x' = x + 1
handleKeys (EventKey (Char 's') pos _ _) state = if pos == Down 
  then moveDown state else state
handleKeys _ state = state

canUpdate :: State -> Bool
canUpdate state = (boundColision $ blockCoordList state) && not(mapColision state (blockCoordList state))

boundColision :: [(Float,Float)] -> Bool
boundColision [] = True
boundColision (x:xs) = if fst x > 9 || fst x < 0 then False 
    else boundColision xs

moveDown :: State -> State
moveDown state = state {blockPos = (x,y + n)}
  where 
    (x,y) = blockPos state
    n = countMovingDown state 0

countMovingDown :: State -> Float -> Float
countMovingDown state n = if isNotColision (state {blockPos = (x,y + n)})
  then countMovingDown state (n + 1) else n - 1
    where 
      (x,y) = blockPos state
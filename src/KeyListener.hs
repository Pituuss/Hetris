module KeyListener
  ( handleKeys
  ) where

import Graphics.Gloss.Interface.Pure.Game
import State
import GameLogic

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
handleKeys _ state = state

canUpdate :: State -> Bool
canUpdate state = (boundColision $ blockCoordList state) && not(mapColision state (blockCoordList state))

boundColision :: [(Float,Float)] -> Bool
boundColision [] = True
boundColision (x:xs) = if fst x > 9 || fst x < 0 then False 
    else boundColision xs

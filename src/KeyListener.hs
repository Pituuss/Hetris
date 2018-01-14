module KeyListener
  ( handleKeys
  ) where

import Graphics.Gloss.Interface.Pure.Game
import State

handleKeys :: Event -> State -> State
handleKeys (EventKey (Char 'a') _ _ _) state =
  if canUpdate (state {blockPos = (x', y)})
    then state {blockPos = (x', y)}
    else state
  where
    (x, y) = blockPos state
    x' = x - 1
handleKeys (EventKey (Char 'd') _ _ _) state =
  if canUpdate (state {blockPos = (x', y)})
    then state {blockPos = (x', y)}
    else state
  where
    (x, y) = blockPos state
    x' = x + 1
handleKeys _ state = state

canUpdate :: State -> Bool
canUpdate state = fst (blockPos state) > 0 || fst (blockPos state) < 10

module GameLogic
  ( simpleFalling
  ) where

import Graphics.Gloss.Data.ViewPort
import State

simpleFalling :: ViewPort -> Float -> State -> State
simpleFalling _ = moveBlock

moveBlock :: Float -> State -> State
moveBlock seconds state = state {blockPos = (x, y')}
  where
    (x, y) = blockPos state
    y'
      -- if seconds == 1
     = y + 1
        -- else y

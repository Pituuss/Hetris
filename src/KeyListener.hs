module KeyListener
    (
    handleKeys
    ) where

import State
import Graphics.Gloss.Interface.Pure.Game

handleKeys :: Event -> State -> State
handleKeys (EventKey (Char 'a') _ _ _) state = if canUpdate (state {blockPos = (x',y)}) then state {blockPos = (x',y)}
else state
    where 
        (x,y) = blockPos state
        x' = x - 1
handleKeys (EventKey (Char 'd') _ _ _) state = if canUpdate (state {blockPos = (x',y)}) then state {blockPos = (x',y)}
        else state
            where 
                (x,y) = blockPos state
                x' = x + 1
handleKeys _ state = state

canUpdate :: State -> Bool
canUpdate state = if (\(a,b) -> a) (blockPos state) < 0 || (\(a,b) -> b) (blockPos state) > 10 
    then False else True
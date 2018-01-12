module Main where

import Blocks
import BoardRenderer
import GameBoard
import GameLogic
import Graphics.Gloss
import State

window :: Display
window = InWindow "Hetris" (1024, 768) (0, 0)

background :: Color
background = black

main :: IO ()
main = simulate window white 1 initialGameState render simpleFalling

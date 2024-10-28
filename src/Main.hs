module Main where

import Graphics.Gloss.Interface.IO.Game
import Model
import View
import Controller

-- Set up the display
window :: Display
window = InWindow "Pacman" (800, 600) (100, 100)

background :: Color
background = black

-- Run the game
main :: IO ()
main = playIO window background 60 initialState render handleInput (const return)

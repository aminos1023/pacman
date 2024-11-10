module Main where

import Graphics.Gloss.Interface.IO.Game
import Model
import View
import Controller

-- Set up the display
window :: Display
window = InWindow "Pac-Man" (800, 600) (100, 100)

-- Fixed background color (will be overridden in render)
background :: Color
background = black

-- Run the game
main :: IO ()
main = do
    gameState <- initialState  -- initialState is in IO to generate random pellets
    playIO window background 60 gameState render handleInput update

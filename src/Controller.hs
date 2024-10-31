module Controller where

import Graphics.Gloss.Interface.IO.Game
import Model

-- Handle input events
handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game = pure $ setPacManDirection DirUp game
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game = pure $ setPacManDirection DirDown game
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game = pure $ setPacManDirection DirLeft game
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game = pure $ setPacManDirection DirRight game
handleInput _ game = pure game

-- Set PacMan's direction
setPacManDirection :: Direction -> GameState -> GameState
setPacManDirection dir game = game { pacMan = (pacMan game) { direction = dir } }

-- Update game state
update :: Float -> GameState -> IO GameState
update deltaTime game = pure $ movePacMan deltaTime game

-- Move PacMan based on his direction
movePacMan :: Float -> GameState -> GameState
movePacMan deltaTime game = game { pacMan = updatedPacMan }
  where
    speed = 100  -- Units per second
    PacMan x y dir = pacMan game
    (dx, dy) = case dir of
      DirUp    -> (0, speed * deltaTime)
      DirDown  -> (0, -speed * deltaTime)
      DirLeft  -> (-speed * deltaTime, 0)
      DirRight -> (speed * deltaTime, 0)
      None     -> (0, 0)
    newX = x + dx
    newY = y + dy
    updatedPacMan = PacMan newX newY dir

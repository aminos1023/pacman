module Controller where

import Graphics.Gloss.Interface.IO.Game
import Model

-- Handle input events
handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game = pure $ movePacMan 0 10 game
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game = pure $ movePacMan 0 (-10) game
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game = pure $ movePacMan (-10) 0 game
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game = pure $ movePacMan 10 0 game
handleInput _ game = pure game


-- Move PacMan
-- Move PacMan
movePacMan :: Float -> Float -> GameState -> GameState
movePacMan dx dy game = game { pacMan = PacMan newPacX newPacY }
  where
    PacMan currentX currentY = pacMan game
    newPacX = currentX + dx
    newPacY = currentY + dy

-- Update game state
update :: Float -> GameState -> GameState
update _ game = game  -- No updates for now, just return the current state

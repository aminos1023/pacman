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
update deltaTime game = pure $ moveGameObjects deltaTime game

-- Move game objects
moveGameObjects :: Float -> GameState -> GameState
moveGameObjects deltaTime game = game 
    { pacMan = movePacMan deltaTime (pacMan game)
    , ghosts = map (moveGhost deltaTime (pacMan game)) (ghosts game)
    }

-- Move PacMan based on his direction
movePacMan :: Float -> PacMan -> PacMan
movePacMan deltaTime pacman@(PacMan x y dir) = pacman { pacX = newX, pacY = newY }
  where
    speed = 100.0  -- Units per second
    (dx, dy) = directionToDelta dir speed deltaTime
    newX = x + dx
    newY = y + dy

-- Move a ghost
moveGhost :: Float -> PacMan -> Ghost -> Ghost
moveGhost deltaTime pacman ghost@(Ghost x y _ gType) = ghost { 
    ghostX = newX, 
    ghostY = newY, 
    ghostDirection = newDir 
  }
  where
    speed = 80.0  -- Units per second
    newDir = case gType of
      Blinky -> blinkyBehavior pacman ghost
      Pinky  -> pinkyBehavior pacman ghost
      Inky   -> inkyBehavior pacman ghost
      Clyde  -> clydeBehavior pacman ghost
    (dx, dy) = directionToDelta newDir speed deltaTime
    newX = x + dx
    newY = y + dy

-- Blinky's behavior: Chases Pac-Man directly
blinkyBehavior :: PacMan -> Ghost -> Direction
blinkyBehavior (PacMan px py _) (Ghost x y _ _) = newDir
  where
    deltaX = px - x
    deltaY = py - y
    newDir = chooseDirection deltaX deltaY

-- Pinky's behavior: Attempts to ambush Pac-Man
pinkyBehavior :: PacMan -> Ghost -> Direction
pinkyBehavior (PacMan px py pDir) (Ghost x y _ _) = newDir
  where
    offset = 80.0  -- Adjust offset as needed
    (targetX, targetY) = case pDir of
      DirUp    -> (px, py + offset)
      DirDown  -> (px, py - offset)
      DirLeft  -> (px - offset, py)
      DirRight -> (px + offset, py)
      None     -> (px, py)
    deltaX = targetX - x
    deltaY = targetY - y
    newDir = chooseDirection deltaX deltaY

-- Inky's behavior: For simplicity, we'll make Inky continue in the same direction
inkyBehavior :: PacMan -> Ghost -> Direction
inkyBehavior _ ghost = ghostDirection ghost

-- Clyde's behavior: Switches between chasing and wandering
clydeBehavior :: PacMan -> Ghost -> Direction
clydeBehavior pacman ghost@(Ghost x y _ _) = 
    if distance > 100.0
    then blinkyBehavior pacman ghost
    else wanderBehavior ghost
  where
    PacMan px py _ = pacman
    distance = sqrt ((px - x) * (px - x) + (py - y) * (py - y))

-- Choose direction based on deltas
chooseDirection :: Float -> Float -> Direction
chooseDirection deltaX deltaY
  | abs deltaX > abs deltaY = if deltaX > 0 then DirRight else DirLeft
  | otherwise               = if deltaY > 0 then DirUp else DirDown

-- Wander behavior for Clyde
wanderBehavior :: Ghost -> Direction
wanderBehavior ghost = ghostDirection ghost  -- Keep current direction

-- Convert direction to movement delta
directionToDelta :: Direction -> Float -> Float -> (Float, Float)
directionToDelta dir speed deltaTime = case dir of
  DirUp    -> (0, speed * deltaTime)
  DirDown  -> (0, -speed * deltaTime)
  DirLeft  -> (-speed * deltaTime, 0)
  DirRight -> (speed * deltaTime, 0)
  None     -> (0, 0)

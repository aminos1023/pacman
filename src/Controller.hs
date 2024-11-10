module Controller where

import Graphics.Gloss.Interface.IO.Game
import Model
import Data.List (minimumBy, delete)
import Data.Function (on)

-- Handle input events
handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game
  | not (isPaused game) = pure $ setPacManDirection DirUp game
  | otherwise = pure game
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game
  | not (isPaused game) = pure $ setPacManDirection DirDown game
  | otherwise = pure game
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game
  | not (isPaused game) = pure $ setPacManDirection DirLeft game
  | otherwise = pure game
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game
  | not (isPaused game) = pure $ setPacManDirection DirRight game
  | otherwise = pure game
handleInput (EventKey (SpecialKey KeySpace) Down _ _) game = pure $ togglePause game
handleInput (EventKey (Char 'p') Down _ _) game = pure $ togglePause game
handleInput _ game = pure game

-- Toggle the game's paused state
togglePause :: GameState -> GameState
togglePause game = game { isPaused = not (isPaused game) }

-- Set Pac-Man's direction
setPacManDirection :: Direction -> GameState -> GameState
setPacManDirection dir game = game { pacMan = (pacMan game) { direction = dir } }

-- Update game state
update :: Float -> GameState -> IO GameState
update deltaTime game
  | isPaused game = pure game  -- Do not update if the game is paused
  | otherwise = pure $ updateGameState deltaTime game

-- Update the game state, including moving objects and handling collisions
updateGameState :: Float -> GameState -> GameState
updateGameState deltaTime game =
    let gameAfterMovement = moveGameObjects deltaTime game
        gameAfterPelletCollection = handlePelletCollection gameAfterMovement
    in gameAfterPelletCollection

-- Move game objects
moveGameObjects :: Float -> GameState -> GameState
moveGameObjects deltaTime game = game
    { pacMan = movePacMan deltaTime (pacMan game) (walls game)
    , ghosts = map (moveGhost deltaTime (pacMan game) (walls game)) (ghosts game)
    }

-- Move Pac-Man based on his direction
movePacMan :: Float -> PacMan -> [Wall] -> PacMan
movePacMan deltaTime pacman@(PacMan x y r dir) walls
  | collidesWithWalls newX newY r walls = pacman  -- Stop movement if collision detected
  | otherwise = pacman { pacX = newX, pacY = newY }
  where
    speed = 100.0  -- Units per second
    (dx, dy) = directionToDelta dir speed deltaTime
    newX = x + dx
    newY = y + dy

-- Handle pellet collection
handlePelletCollection :: GameState -> GameState
handlePelletCollection game =
    let pac = pacMan game
        remainingPellets = filter (not . pacManCollidesWithPellet pac) (pellets game)
        collectedPellets = length (pellets game) - length remainingPellets
        newScore = score game + (collectedPellets * 100)
    in game { pellets = remainingPellets, score = newScore }

-- Check if Pac-Man collides with a pellet
pacManCollidesWithPellet :: PacMan -> Pellet -> Bool
pacManCollidesWithPellet pac pellet =
    let distanceBetween = distance (pacX pac) (pacY pac) (pelletX pellet) (pelletY pellet)
    in distanceBetween < (pacRadius pac + 5)  -- 5 is the pellet radius

-- Move a ghost
moveGhost :: Float -> PacMan -> [Wall] -> Ghost -> Ghost
moveGhost deltaTime pacman walls ghost@(Ghost x y r dir gType) =
    let speed = 80.0  -- Units per second
        -- Attempt to move in the current direction
        (dx, dy) = directionToDelta dir speed deltaTime
        newX = x + dx
        newY = y + dy
        collision = collidesWithWalls newX newY r walls
    in if collision
       then
           -- Collision detected, bounce off by reversing direction
           let bouncedDir = oppositeDirection dir
               (dxBounce, dyBounce) = directionToDelta bouncedDir speed deltaTime
               bounceX = x + dxBounce
               bounceY = y + dyBounce
               bounceCollision = collidesWithWalls bounceX bounceY r walls
           in if bounceCollision
              then
                  -- Can't move in opposite direction either, stay in place
                  ghost
              else
                  -- Update position and direction after bouncing
                  ghost { ghostX = bounceX, ghostY = bounceY, ghostDirection = bouncedDir }
       else
           -- No collision, proceed with AI direction
           let aiDir = determineDirection ghost pacman walls
               -- Avoid immediate reversal by excluding opposite direction
               aiDir' = if aiDir == oppositeDirection dir then dir else aiDir
               (dxAi, dyAi) = directionToDelta aiDir' speed deltaTime
               updatedX = x + dxAi
               updatedY = y + dyAi
               aiCollision = collidesWithWalls updatedX updatedY r walls
           in if aiCollision
              then
                  -- Can't move in AI direction, continue in current direction
                  ghost { ghostX = newX, ghostY = newY }
              else
                  -- Move and update direction to AI direction
                  ghost { ghostX = updatedX, ghostY = updatedY, ghostDirection = aiDir' }

-- Determine ghost's direction based on its type and walls
determineDirection :: Ghost -> PacMan -> [Wall] -> Direction
determineDirection ghost pacman walls = newDir
  where
    (targetX, targetY) = case ghostType ghost of
      Blinky -> (pacX pacman, pacY pacman)
      Pinky  -> getPinkyTarget pacman
      Inky   -> getInkyTarget pacman ghost
      Clyde  -> getClydeTarget pacman ghost
    possibleDirs = [DirUp, DirDown, DirLeft, DirRight]
    -- Exclude opposite direction to prevent immediate reversal
    dirsExcludingOpposite = delete (oppositeDirection (ghostDirection ghost)) possibleDirs
    validDirs = filter (\d -> not (willCollide (ghostX ghost, ghostY ghost) d walls (ghostRadius ghost))) dirsExcludingOpposite
    newDir = if null validDirs
             then ghostDirection ghost  -- No valid moves, keep current direction
             else chooseBestDirection (ghostX ghost, ghostY ghost) (targetX, targetY) validDirs

-- Check if moving in a direction will cause collision
willCollide :: (Float, Float) -> Direction -> [Wall] -> Float -> Bool
willCollide (x, y) dir walls r = collidesWithWalls newX newY r walls
  where
    speed = 2.0  -- Small step to check collision
    deltaTime = 0.016  -- Assume 60 FPS
    (dx, dy) = directionToDelta dir speed deltaTime
    newX = x + dx
    newY = y + dy

-- Choose the best direction towards the target
chooseBestDirection :: (Float, Float) -> (Float, Float) -> [Direction] -> Direction
chooseBestDirection (x, y) (tx, ty) dirs = fst $ minimumBy (compare `on` snd) distances
  where
    distances = [(dir, distToTarget dir) | dir <- dirs]
    distToTarget dir =
      let (dx, dy) = directionToDelta dir 1.0 1.0
      in distance (x + dx) (y + dy) tx ty

-- Get Pinky's target (ahead of Pac-Man)
getPinkyTarget :: PacMan -> (Float, Float)
getPinkyTarget (PacMan px py _ pDir) = case pDir of
  DirUp    -> (px, py + offset)
  DirDown  -> (px, py - offset)
  DirLeft  -> (px - offset, py)
  DirRight -> (px + offset, py)
  None     -> (px, py)
  where offset = 80.0

-- Get Inky's target (mirror of Pac-Man's position)
getInkyTarget :: PacMan -> Ghost -> (Float, Float)
getInkyTarget (PacMan px py _ _) (Ghost gx gy _ _ _) = (2 * px - gx, 2 * py - gy)

-- Get Clyde's target (chase or scatter)
getClydeTarget :: PacMan -> Ghost -> (Float, Float)
getClydeTarget pacman ghost@(Ghost x y _ _ _) =
  if distance x y (pacX pacman) (pacY pacman) > 100.0
  then (pacX pacman, pacY pacman)
  else (-300, -300)  -- Clyde's scatter corner

-- Get the opposite direction
oppositeDirection :: Direction -> Direction
oppositeDirection DirUp    = DirDown
oppositeDirection DirDown  = DirUp
oppositeDirection DirLeft  = DirRight
oppositeDirection DirRight = DirLeft
oppositeDirection None     = None

-- Convert direction to movement delta
directionToDelta :: Direction -> Float -> Float -> (Float, Float)
directionToDelta dir speed deltaTime = case dir of
  DirUp    -> (0, speed * deltaTime)
  DirDown  -> (0, -speed * deltaTime)
  DirLeft  -> (-speed * deltaTime, 0)
  DirRight -> (speed * deltaTime, 0)
  None     -> (0, 0)

-- Note: Use 'distance' from Model.hs

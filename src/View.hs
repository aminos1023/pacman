module View where

import Graphics.Gloss
import Model

-- Render state of game
render :: GameState -> IO Picture
render game
  | isGameOver game = pure $ drawGameOverScreen game
  | otherwise = pure $ pictures
        [ drawWalls (walls game) (wallColor $ colorScheme game)
        , drawPellets (pellets game)
        , drawGhosts (ghosts game)
        , drawPacMan (pacMan game)
        , drawScore (score game) (scoreColor $ colorScheme game)
        , drawLevel (level game) (scoreColor $ colorScheme game)
        , drawPauseOverlay game
        ]

-- Draw Pac-Man
drawPacMan :: PacMan -> Picture
drawPacMan pacman = translate (pacX pacman) (pacY pacman) $ color yellow $ circleSolid 10

-- Draw Ghosts
drawGhosts :: [Ghost] -> Picture
drawGhosts = pictures . map drawGhost

drawGhost :: Ghost -> Picture
drawGhost (Ghost x y r _ gType) = translate x y $ color ghostColor $ ghostShape
  where
    ghostColor = case gType of
      Blinky -> red
      Pinky  -> rose
      Inky   -> azure
      Clyde  -> orange
    ghostShape = circleSolid r

-- Draw Pellets
drawPellets :: [Pellet] -> Picture
drawPellets = pictures . map (\(Pellet x y) -> translate x y $ color white $ circleSolid 5)

-- Draw Walls
drawWalls :: [Wall] -> Color -> Picture
drawWalls walls wallCol = pictures $ map (drawWall wallCol) walls

drawWall :: Color -> Wall -> Picture
drawWall wallCol (Wall x y w h) = translate x y $ color wallCol $ rectangleSolid w h

-- Draw the pause overlay if the game is paused
drawPauseOverlay :: GameState -> Picture
drawPauseOverlay game
  | isPaused game = translate (-100) 0 $ color white $ scale 0.3 0.3 $ text "Paused"
  | otherwise = blank

-- Draw the score at the top right corner
drawScore :: Int -> Color -> Picture
drawScore sc scoreCol = translate posX posY $ color scoreCol $ scale 0.15 0.15 $ text ("Score: " ++ show sc)
  where
    posX = 250   -- Adjust these values based on your window size
    posY = 250

-- Draw the level at the top left corner
drawLevel :: Int -> Color -> Picture
drawLevel lvl scoreCol = translate posX posY $ color scoreCol $ scale 0.15 0.15 $ text ("Level: " ++ show lvl)
  where
    posX = -380   -- Adjust these values based on your window size
    posY = 250

-- Draw the game over screen
drawGameOverScreen :: GameState -> Picture
drawGameOverScreen game = pictures
    [ color white $ translate (-100) 0 $ scale 0.3 0.3 $ text "Game Over"
    , color white $ translate (-140) (-50) $ scale 0.2 0.2 $ text ("Final Score: " ++ show (score game))
    ]


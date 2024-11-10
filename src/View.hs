module View where

import Graphics.Gloss
import Model

-- Render state of game
render :: GameState -> IO Picture
render game = pure $ pictures
    [ drawWalls (walls game)    -- Draw walls first
    , drawPellets (pellets game)
    , drawGhosts (ghosts game)
    , drawPacMan (pacMan game)
    , drawScore (score game)    -- Display the score
    , drawPauseOverlay game     -- Display pause message if game is paused
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
drawWalls :: [Wall] -> Picture
drawWalls = pictures . map drawWall

drawWall :: Wall -> Picture
drawWall (Wall x y w h) = translate x y $ color blue $ rectangleSolid w h

-- Draw the pause overlay if the game is paused
drawPauseOverlay :: GameState -> Picture
drawPauseOverlay game
  | isPaused game = translate (-100) 0 $ color white $ scale 0.3 0.3 $ text "Paused"
  | otherwise = blank

-- Draw the score at the top right corner
drawScore :: Int -> Picture
drawScore sc = translate posX posY $ color white $ scale 0.15 0.15 $ text ("Score: " ++ show sc)
  where
    posX = 250   -- Adjust these values based on your window size
    posY = 250

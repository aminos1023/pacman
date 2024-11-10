module View where

import Graphics.Gloss
import Model

-- Render state of game
render :: GameState -> IO Picture
render game = pure $ pictures [drawPacMan (pacMan game), drawGhosts (ghosts game), drawPellets (pellets game), drawWalls (walls game)]

-- Draw PacMan
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
drawWall (Wall x y w h) = translate x y $ color blue $ rectangleWire w h


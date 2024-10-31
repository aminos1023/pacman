module View where

import Graphics.Gloss
import Model

-- Display the current state of the game
render :: GameState -> IO Picture
render game = pure $ pictures [drawPacMan (pacMan game), drawGhosts (ghosts game), drawPellets (pellets game)]

-- Draw PacMan
drawPacMan :: PacMan -> Picture
drawPacMan pacman = translate (pacX pacman) (pacY pacman) $ color yellow $ circleSolid 10

-- Draw Ghosts
drawGhosts :: [Ghost] -> Picture
drawGhosts = pictures . map drawGhost

drawGhost :: Ghost -> Picture
drawGhost (Ghost x y _ gType) = translate x y $ color ghostColor $ ghostShape
  where
    ghostColor = case gType of
      Blinky -> red
      Pinky  -> rose
      Inky   -> azure
      Clyde  -> orange
    ghostShape = circleSolid 10  -- Simple circle for the ghost; you can replace this with a more detailed shape

-- Draw Pellets
drawPellets :: [Pellet] -> Picture
drawPellets = pictures . map (\(Pellet x y) -> translate x y $ color white $ circleSolid 5)

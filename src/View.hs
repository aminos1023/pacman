module View where

import Graphics.Gloss
import Model

-- Display the current state of the game
render :: GameState -> IO Picture
render game = pure $ pictures [drawPacMan (pacMan game), drawGhosts (ghosts game), drawPellets (pellets game)]


-- Draw PacMan
drawPacMan :: PacMan -> Picture
drawPacMan (PacMan x y) = translate x y $ color yellow $ circleSolid 10

-- Draw Ghosts
drawGhosts :: [Ghost] -> Picture
drawGhosts = pictures . map (\(Ghost x y) -> translate x y $ color red $ circleSolid 10)

-- Draw Pellets
drawPellets :: [Pellet] -> Picture
drawPellets = pictures . map (\(Pellet x y) -> translate x y $ color blue $ circleSolid 5)

module Model where

import System.Random (StdGen, mkStdGen)

-- Define Direction type
data Direction = DirUp | DirDown | DirLeft | DirRight | None deriving (Eq, Show)

-- Define GhostType
data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Show)

-- Define Ghost with additional fields
data Ghost = Ghost { 
    ghostX :: Float, 
    ghostY :: Float, 
    ghostDirection :: Direction,
    ghostType :: GhostType
}

-- Define PacMan
data PacMan = PacMan { 
    pacX :: Float, 
    pacY :: Float, 
    direction :: Direction
}

-- Define Pellet
data Pellet = Pellet { pelletX :: Float, pelletY :: Float }

-- GameState definition
data GameState = GameState 
    { pacMan   :: PacMan
    , ghosts   :: [Ghost]
    , pellets  :: [Pellet]
    , score    :: Int
    , isPaused :: Bool
    , rngState :: StdGen
    }

-- Initial game state
initialState :: GameState
initialState = GameState 
    { pacMan = PacMan 0 0 None
    , ghosts = 
        [ Ghost (-100) 100 DirLeft Blinky
        , Ghost 100 100 DirDown Pinky
        , Ghost (-100) (-100) DirUp Inky
        , Ghost 100 (-100) DirRight Clyde
        ]
    , pellets = [Pellet 50 50, Pellet (-50) (-50)]
    , score = 0
    , isPaused = False
    , rngState = mkStdGen 42  -- Seeded RNG for now
    }

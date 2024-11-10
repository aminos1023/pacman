module Model where

import System.Random (StdGen, mkStdGen)

-- Define Direction type
data Direction = DirUp | DirDown | DirLeft | DirRight | None deriving (Eq, Show)

-- Define GhostType
data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Show)

-- Define Ghost
data Ghost = Ghost {
    ghostX         :: Float,
    ghostY         :: Float,
    ghostRadius    :: Float,
    ghostDirection :: Direction,
    ghostType :: GhostType
}

-- Define PacMan
data PacMan = PacMan {
    pacX      :: Float,
    pacY      :: Float,
    pacRadius :: Float,
    direction :: Direction
}

-- Define Pellet
data Pellet = Pellet { pelletX :: Float, pelletY :: Float }

-- Define Wall
data Wall = Wall { wallX :: Float, wallY :: Float, wallWidth :: Float, wallHeight :: Float }

-- GameState definition
data GameState = GameState
    { pacMan   :: PacMan
    , ghosts   :: [Ghost]
    , pellets  :: [Pellet]
    , walls    :: [Wall]
    , score    :: Int
    , isPaused :: Bool
    , rngState :: StdGen
    }

-- Initial game state
initialState :: GameState
initialState = GameState
    { pacMan = PacMan 0 (-100) 10 None
    , ghosts =
        [ Ghost 0 0 10 DirLeft Blinky
        , Ghost 0 0 10 DirDown Pinky
        , Ghost 0 0 10 DirUp Inky
        , Ghost 0 0 10 DirRight Clyde
        ]
    , pellets = [Pellet 50 50, Pellet (-50) (-50)]
    , walls = pacmanMaze
    , score = 0
    , isPaused = False
    , rngState = mkStdGen 42  -- Seeded RNG for now
    }

--The maze model
pacmanMaze :: [Wall]
pacmanMaze =
    [ Wall 0 300 800 0
    , Wall (-400) 0 0 600
    , Wall 0 (-300) 800 0
    , Wall 400 0 0 600  -- The whole canvas made of 4 walls
    {- , Wall (-50) 0 0 100
    , Wall 0 (-50) 100 0
    , Wall 50 0 0 100 -- Ghosts pen   -}
    , Wall 0 200 200 20
    , Wall 0 (-200) 200 20
    , Wall (-300) 200 20 100
    , Wall (-300) 0 50 20
    , Wall 100 100 200 20
    , Wall 100 (-100) 20 100
    , Wall 300 50 20 100
    , Wall 280 (-160) 100 20
    , Wall (-200) (-200) 20 100
    
    ]

-- Collision detection for PacMan and Ghosts
collidesWithWalls :: Float -> Float -> Float -> [Wall] -> Bool
collidesWithWalls x y r = any (\(Wall wx wy ww wh) ->
    let halfW = ww / 2
        halfH = wh / 2
    in abs (x - wx) < (halfW + r) && abs (y - wy) < (halfH + r))

-- Check if PacMan collides with any walls
pacManCollidesWithWalls :: PacMan -> [Wall] -> Bool
pacManCollidesWithWalls (PacMan px py pr _) = collidesWithWalls px py pr

-- Check if any Ghost collides with any walls
ghostCollidesWithWalls :: Ghost -> [Wall] -> Bool
ghostCollidesWithWalls (Ghost gx gy gr _ _) = collidesWithWalls gx gy gr

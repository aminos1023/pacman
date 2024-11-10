module Model where  -- Export all definitions by default

import System.Random (StdGen, mkStdGen, Random(randomR))
import Data.List (any)

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
    ghostType      :: GhostType
}

-- Define Pac-Man
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

-- Initial game state (now in IO monad to generate random pellets)
initialState :: IO GameState
initialState = do
    let rng = mkStdGen 42
    let pacman = PacMan 0 (-240) 10 DirRight
    let ghosts = [ Ghost (-60) 0 10 DirLeft Blinky
                 , Ghost (-20) 0 10 DirDown Pinky
                 , Ghost 20 0 10 DirUp Inky
                 , Ghost 60 0 10 DirRight Clyde
                 ]
    let walls = pacmanMaze
    let initialPositions = (map (\g -> (ghostX g, ghostY g)) ghosts) ++ [(pacX pacman, pacY pacman)]
    let occupiedPositions = initialPositions
    (pellets, rng') <- generatePellets rng walls occupiedPositions 10
    return GameState { pacMan = pacman
                     , ghosts = ghosts
                     , pellets = pellets
                     , walls = walls
                     , score = 0
                     , isPaused = False
                     , rngState = rng'
                     }

-- Generate a specified number of pellets at random positions
generatePellets :: StdGen -> [Wall] -> [(Float, Float)] -> Int -> IO ([Pellet], StdGen)
generatePellets rng walls occupiedPositions n = go rng n [] occupiedPositions
  where
    go gen 0 acc _ = return (acc, gen)
    go gen count acc occupied = do
        let (x, gen1) = randomR (-380, 380) gen
        let (y, gen2) = randomR (-260, 260) gen1
        if isWallAt x y walls || isOccupied x y occupied
            then go gen2 count acc occupied  -- Retry without decreasing count
            else go gen2 (count - 1) (Pellet x y : acc) ((x, y) : occupied)
    isOccupied x y positions = any (\(ox, oy) -> distance x y ox oy < 20) positions

-- Check if there is a wall at a given position
isWallAt :: Float -> Float -> [Wall] -> Bool
isWallAt x y walls = any (pointInsideWall x y) walls

-- Check if a point is inside a wall
pointInsideWall :: Float -> Float -> Wall -> Bool
pointInsideWall x y Wall{wallX=wx, wallY=wy, wallWidth=ww, wallHeight=wh} =
    x > (wx - ww / 2) && x < (wx + ww / 2) &&
    y > (wy - wh / 2) && y < (wy + wh / 2)

-- Calculate Euclidean distance
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

-- The maze model with additional walls on the left and right sides
pacmanMaze :: [Wall]
pacmanMaze =
    -- Outer boundaries
    [ Wall 0 280 800 20   -- Top boundary
    , Wall (-390) 0 20 600 -- Left boundary
    , Wall 0 (-280) 800 20 -- Bottom boundary
    , Wall 390 0 20 600   -- Right boundary
    ]
    ++
    -- Inner walls
    [ Wall 0 200 160 20    -- Horizontal wall near top
    , Wall (-80) 140 20 120 -- Vertical wall on left
    , Wall (80) 140 20 120  -- Vertical wall on right
    , Wall 0 80 160 20     -- Horizontal wall below top walls
    , Wall (-160) 0 20 200 -- Vertical wall on far left
    , Wall (160) 0 20 200  -- Vertical wall on far right
    , Wall 0 (-80) 160 20  -- Horizontal wall above bottom
    , Wall (-80) (-140) 20 120 -- Vertical wall on left below
    , Wall (80) (-140) 20 120  -- Vertical wall on right below
    , Wall 0 (-200) 160 20  -- Horizontal wall at bottom
    -- Additional walls on the left side
    , Wall (-240) 160 80 20    -- Left top horizontal wall
    , Wall (-280) 120 20 80    -- Left vertical wall
    , Wall (-240) 80 80 20     -- Left middle horizontal wall
    , Wall (-280) 0 20 160     -- Left vertical wall extending down
    , Wall (-240) (-80) 80 20  -- Left bottom horizontal wall
    -- Additional walls on the right side
    , Wall 240 160 80 20       -- Right top horizontal wall
    , Wall 280 120 20 80       -- Right vertical wall
    , Wall 240 80 80 20        -- Right middle horizontal wall
    , Wall 280 0 20 160        -- Right vertical wall extending down
    , Wall 240 (-80) 80 20     -- Right bottom horizontal wall
    ]

-- Collision detection for Pac-Man and Ghosts
collidesWithWalls :: Float -> Float -> Float -> [Wall] -> Bool
collidesWithWalls x y r walls = any (\Wall{wallX=wx, wallY=wy, wallWidth=ww, wallHeight=wh} ->
    let halfW = ww / 2 + r
        halfH = wh / 2 + r
    in abs (x - wx) < halfW && abs (y - wy) < halfH) walls

-- Check if Pac-Man collides with any walls
pacManCollidesWithWalls :: PacMan -> [Wall] -> Bool
pacManCollidesWithWalls (PacMan px py pr _) walls = collidesWithWalls px py pr walls

-- Check if any Ghost collides with any walls
ghostCollidesWithWalls :: Ghost -> [Wall] -> Bool
ghostCollidesWithWalls (Ghost gx gy gr _ _) walls = collidesWithWalls gx gy gr walls

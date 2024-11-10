module Model where

import System.Random (StdGen, mkStdGen, Random(randomR))
import Data.List (any)
import Graphics.Gloss (Color, makeColor, black, blue, white)
import Graphics.Gloss.Interface.IO.Game (Display(InWindow))

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

-- Color Scheme
data ColorScheme = ColorScheme {
    wallColor        :: Color,
    backgroundColor  :: Color,
    scoreColor       :: Color
}

-- GameState definition
data GameState = GameState
    { pacMan       :: PacMan
    , ghosts       :: [Ghost]
    , pellets      :: [Pellet]
    , walls        :: [Wall]
    , score        :: Int
    , level        :: Int
    , isPaused     :: Bool
    , isGameOver   :: Bool
    , colorScheme  :: ColorScheme
    , rngState     :: StdGen
    }

-- Initial game state
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
    let initialPositions = map (\g -> (ghostX g, ghostY g)) ghosts ++ [(pacX pacman, pacY pacman)]
    let occupiedPositions = initialPositions
    (pellets, rng') <- generatePellets rng walls occupiedPositions 10
    let initialColorScheme = ColorScheme {
            wallColor = blue,
            backgroundColor = black,
            scoreColor = white
        }
    return GameState { pacMan = pacman
                     , ghosts = ghosts
                     , pellets = pellets
                     , walls = walls
                     , score = 0
                     , level = 1
                     , isPaused = False
                     , isGameOver = False
                     , colorScheme = initialColorScheme
                     , rngState = rng'
                     }

-- Generate a specified number of pellets at random positions
generatePellets :: StdGen -> [Wall] -> [(Float, Float)] -> Int -> IO ([Pellet], StdGen)
generatePellets rng walls occupiedPositions n = go rng n [] occupiedPositions
  where
    go gen 0 acc _ = return (acc, gen)
    go gen count acc occupied = do
        let (x, gen1) = randomR (-380, 380) gen
            (y, gen2) = randomR (-260, 260) gen1
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
    [ 
    -- Outer boundaries
      Wall 0 280 800 20       -- Top boundary
    , Wall (-390) 0 20 600    -- Left boundary
    , Wall 0 (-280) 800 20    -- Bottom boundary
    , Wall 390 0 20 600       -- Right boundary

    -- Inner walls (your given walls)
    , Wall (-200) 200 120 20    -- Top-left horizontal wall
    , Wall (200) 200 120 20     -- Top-right horizontal wall
    , Wall (-260) 140 20 120    -- Top-left vertical wall
    , Wall (260) 140 20 120     -- Top-right vertical wall
    , Wall (-200) (-200) 120 20 -- Bottom-left horizontal wall
    , Wall (200) (-200) 120 20  -- Bottom-right horizontal wall
    , Wall (-260) (-140) 20 120 -- Bottom-left vertical wall
    , Wall (260) (-140) 20 120  -- Bottom-right vertical wall
    , Wall 0 100 200 20         -- Middle-top horizontal wall
    , Wall 0 (-100) 200 20      -- Middle-bottom horizontal wall

    -- Additional walls that do not close off the middle
    , Wall (-150) 50 100 20     -- Left-upper horizontal wall
    , Wall (-150) (-50) 100 20  -- Left-lower horizontal wall
    , Wall (-200) 0 20 100      -- Left vertical wall connecting the two horizontals
    , Wall (150) 50 100 20      -- Right-upper horizontal wall
    , Wall (150) (-50) 100 20   -- Right-lower horizontal wall
    , Wall (200) 0 20 100       -- Right vertical wall connecting the two horizontals
    , Wall (-100) 150 20 100    -- Upper-left vertical wall
    , Wall (100) 150 20 100     -- Upper-right vertical wall
    , Wall (-100) (-150) 20 100 -- Lower-left vertical wall
    , Wall (100) (-150) 20 100  -- Lower-right vertical wall
    , Wall 0 200 20 80          -- Center-top vertical wall (above middle-top horizontal)
    , Wall (-320) 0 20 200      -- Far-left vertical wall
    , Wall (320) 0 20 200       -- Far-right vertical wall
    ]

-- Collision detection for Pac-Man and Ghosts
collidesWithWalls :: Float -> Float -> Float -> [Wall] -> Bool
collidesWithWalls x y r walls = any collision walls
  where
    collision Wall{wallX=wx, wallY=wy, wallWidth=ww, wallHeight=wh} =
        let halfW = ww / 2 + r
            halfH = wh / 2 + r
        in abs (x - wx) < halfW && abs (y - wy) < halfH

-- Check if Pac-Man collides with any walls
pacManCollidesWithWalls :: PacMan -> [Wall] -> Bool
pacManCollidesWithWalls (PacMan px py pr _) walls = collidesWithWalls px py pr walls

-- Check if any Ghost collides with any walls
ghostCollidesWithWalls :: Ghost -> [Wall] -> Bool
ghostCollidesWithWalls (Ghost gx gy gr _ _) walls = collidesWithWalls gx gy gr walls

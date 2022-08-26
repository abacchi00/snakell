module Types where

type Coordinate = (Float, Float)
type SnakeDirection = Coordinate  -- Snake's directon can have four values: (0, -1) = down | (0, 1) = up | (-1, 0) = left | (1, 0) = right
type SnakeBlock = (Coordinate, SnakeDirection) -- Snake's block (x, y) coordinate and (a, b) direction
type BulletState = (Coordinate, SnakeDirection) -- Snake's block (x, y) coordinate and (a, b) direction

data SnakellGame = Game
  { snakeHeadLoc :: Coordinate -- Snake's head (x, y) location
  , snakeTailLoc :: [SnakeBlock] -- Snake's tail blocks (x, y) location
  , snakeDirection :: SnakeDirection -- Snake's directon: (0, -1) = down | (0, 1) = up | (-1, 0) = left | (1, 0) = right, 
  , appleLoc :: Coordinate -- Apple's (x, y) location
  , bullet :: BulletState
  , playerScore :: Float
  , timeAlive :: Int -- seconds
  , timePassed :: Float -- seconds float
  , slimeLocs :: [Coordinate]
  , level :: Int
  , applesEaten :: Int
  , gameRunning :: Bool
  , highScore :: Float
  } deriving Show
module Base where

import Data.Fixed
import Types
import Helpers

newPosIfCollision :: String -> Coordinate -> Coordinate
newPosIfCollision colisionType (x, y)
  | colisionType == "top" || colisionType == "bottom" = (x, (-y))
  | colisionType == "left" || colisionType == "right" = ((-x), y)
  | otherwise = (x, y)

initialHeadLoc :: Coordinate
initialHeadLoc = (blockSize * 5, 0)

initialSnakeDirection :: SnakeDirection
initialSnakeDirection = (0, -1)

restingBulletState :: BulletState
restingBulletState = ((500, 500), (0, 0))

initialSnakeTail :: [SnakeBlock]
initialSnakeTail = firstIncrement ++ secondIncrement ++ thirdIncrement
  where
    firstIncrement = snakeIncrement (initialHeadLoc, initialSnakeDirection)
    secondIncrement = snakeIncrement (last firstIncrement)
    thirdIncrement = snakeIncrement (last secondIncrement)

maxScoring :: Float
maxScoring = 0

initialState :: Float -> SnakellGame
initialState hs = Game
  { snakeHeadLoc = initialHeadLoc
  , snakeTailLoc = initialSnakeTail
  , snakeDirection = initialSnakeDirection
  , appleLoc = (-(blockSize * 3), (blockSize * 3))
  , bullet = restingBulletState
  , playerScore = 0
  , timeAlive = 0
  , timePassed = 0
  , slimeLocs = [(100, 100), (0, 320), (-140, -140)]
  , level = 0
  , applesEaten = 0
  , gameRunning = False
  , highScore = hs
  }

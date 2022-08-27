module Movement where

import Data.Fixed
import Types
import Helpers
import Base
import GHC.Float

moveSnake :: Float -> SnakellGame -> SnakellGame
moveSnake seconds game
  | snakeHitsTail || newScore < 0 = initialState newHighScore
  | gameRunning game = nextGameState 
  | otherwise = initialState newHighScore
  where
    -- Next game state (after each frame)

    nextGameState :: SnakellGame
    nextGameState = Game
      { snakeHeadLoc = (x'', y'')
      , snakeTailLoc = newTailPositions (snakeTailLoc game)
      , snakeDirection = snakeDirection game
      , appleLoc = newAppleLoc
      , bullet = nextBulletState
      , playerScore = newScore
      , timeAlive = newTimeAlive
      , timePassed = newTimePassed
      , slimeLocs = newerSlimeLocs
      , level = newLevel
      , applesEaten = newApplesEaten
      , gameRunning = True
      , highScore = newHighScore
      }

    -- Location and previous direction

    (x, y) = snakeHeadLoc game
    (vx, vy) = snakeDirection game

    -- New locations

    x' = x + vx * snakeTailSpacing
    y' = y + vy * snakeTailSpacing

    collision :: String
    collision = wallCollision (x', y') blockRadius
    (x'', y'') = newPosIfCollision collision (x', y')

    -- New score

    newScore :: Float
    newScore = (playerScore) game + scoreIncrement - scoreDecrement + killedSlimes
      where
        scoreIncrement = if snakeEatsApple then 100 else 0
        scoreDecrement
          | slimeHitsHead = 2
          | slimeHitsTail = 1 
          | otherwise = 0
        killedSlimes = int2Float $ (length (slimeLocs game) - length aliveSlimeLocs) * 10

    -- New High Score
    newHighScore :: Float 
    newHighScore = max newScore (highScore game)


    -- New time alive

    newTimeAlive = timeAlive game + timeAliveIncrement

    timeAliveIncrement = if toInt (timePassed game) < toInt newTimePassed then 1 else 0
    newTimePassed = timePassed game + seconds 

    -- snake hits tail

    snakeHitsTail = length snakeTailHits > 20
    snakeTailHits = filter (==True) [circleCollision (snakeHeadLoc game) blockSize tailBlockLoc blockSize | (tailBlockLoc, _) <- snakeTailLoc game ]

    -- Slime

    newerSlimeLocs = [(x' x, y' y) | (x, y) <- newSlimeLocs]
      where
        (a, b) = snakeHeadLoc game
        x' x = if x < a then x + coordOffset a x else if x == a then x else x - coordOffset a x
        y' y = if y < b then y + coordOffset b y else if y == b then y else y - coordOffset b y
        
        coordOffset v1 v2
          | averageAbs v1 v2 > 200 = 2.5
          | averageAbs v1 v2 > 175 = 2.25
          | averageAbs v1 v2 > 150 = 2.0
          | averageAbs v1 v2 > 125 = 1.75
          | averageAbs v1 v2 > 100 = 1.5
          | averageAbs v1 v2 > 75  = 1.25
          | averageAbs v1 v2 > 50  = 1.0
          | otherwise  = 0.5

        averageAbs v1 v2 = abs ((v1 - v2) / 2)



    newSlimeLocs = if shouldGenerateNewSlime then aliveSlimeLocs ++ [genRandomSlimeLoc x y] else aliveSlimeLocs
      where
        shouldGenerateNewSlime = secondJustPassed && timeToGenerateSlime
        secondJustPassed = timeAlive game /= newTimeAlive
        timeToGenerateSlime = (timeAlive game) `mod` slimeGenerationRatio == 0

    aliveSlimeLocs = filter bulletDidntHitSlime (slimeLocs game)

    bulletDidntHitSlime slimeLoc = not (bulletHitSlime slimeLoc)

    bulletHitSlime slimeLoc = circleCollision bulletLoc 0.01 slimeLoc 25
      where
        (bulletLoc, _) = bullet game

    bulletHitAnySlime = any bulletHitSlime (slimeLocs game)



    slimeHitsTail = not $ null $ filter (==True) [circleCollision slimeLoc blockSize tailBlockLoc blockSize | (tailBlockLoc, _) <- snakeTailLoc game, slimeLoc <- slimeLocs game]

    slimeHitsHead = not $ null $ filter (==True) [circleCollision (snakeHeadLoc game) blockSize slimeLoc blockSize | slimeLoc <- slimeLocs game]
    

    --

    teste :: SnakeBlock -> [SnakeBlock] -> [SnakeBlock]
    teste _ [] = []
    teste _ [a] = if (snakeEatsApple) then snakeIncrement a else []
    teste a (_:xs) = [a] ++ teste (head xs) xs

    newTailPositions :: [SnakeBlock] -> [SnakeBlock]
    newTailPositions xs = [((x, y), snakeDirection game)] ++ teste (head xs) xs

    newAppleLoc :: Coordinate
    newAppleLoc = if snakeEatsApple then genRandomAppleLoc x y else appleLoc game

    genRandomAppleLoc :: Float -> Float -> Coordinate
    genRandomAppleLoc x y = pseudoRandomCoordinate seedX seedY
      where
        seedX = x + 11 + snd (fst (last (snakeTailLoc game)))
        seedY = y + 88 + fst (fst (last (snakeTailLoc game)))

    genRandomSlimeLoc :: Float -> Float -> Coordinate
    genRandomSlimeLoc x y = pseudoRandomCoordinate seedX seedY
      where
        seedX = x + 7 + snd (fst (last (snakeTailLoc game)))
        seedY = y + 48 + fst (fst (last (snakeTailLoc game)))

    snakeEatsApple :: Bool
    snakeEatsApple = circleCollision (appleLoc game) blockRadius (x', y') blockRadius

    nextBulletState = if bulletWallCollision /= "none" || bulletHitAnySlime then restingBulletState else moveBullet
      where
        moveBullet = ((bulletX + bulletDirX * 10, bulletY + bulletDirY * 10), (bulletDirX, bulletDirY))
        ((bulletX, bulletY), (bulletDirX, bulletDirY)) = bullet game
        bulletWallCollision = wallCollision (bulletX, bulletY) (blockRadius / 2)


    newLevel :: Int
    newLevel = if applesEaten game == fib (level game + 1) then (level game) + 1 else level game

    newApplesEaten :: Int
    newApplesEaten
      | newLevel /= level game = 0
      | otherwise = if snakeEatsApple then (applesEaten game) + 1 else applesEaten game

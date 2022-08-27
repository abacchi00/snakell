module Render where

import Graphics.Gloss
import Data.Fixed
import Types
import Helpers

render :: SnakellGame -> [Picture] -> Picture
render game imgs =
  pictures [gameGrass, apple, snakeTail, bulletPic, snakeHead, walls, gameScore, gameHighScore, gameLevel, timeDisplay, slimes, blockScr]

  where

    -- Block Screen
    blockScr :: Picture
    blockScr
      | not (gameRunning game) = uncurry translate (0, 0) $ blockScrPic
      | otherwise = uncurry translate (0, 0) $ emptyScreen
      where
        blockScrPic = scale 1.1 1.1 (imgs !! 5)
        emptyScreen = scale 0 0 (imgs !! 5)

    -- Snake
    snakeHead :: Picture
    snakeHead = uncurry translate headPosition $ rotate headRotation snakeHeadPic
      where
        headRotation
          | snakeDirection game == (0, -1) = 0
          | snakeDirection game == (0, 1)  = 180
          | snakeDirection game == (-1, 0) = 90
          | snakeDirection game == (1, 0)  = 270
        headPosition = snakeHeadLoc game
        snakeHeadPic = scale 0.08 0.08 (imgs !! 2) 

    snakeTail :: Picture
    snakeTail = pictures [uncurry translate (fst tailBlock) $ snakeBlockPic | tailBlock <- snakeTailLoc game]
      where
        snakeBlockPic = scale 0.04 0.04 (imgs !! 3)

    -- Apple

    apple :: Picture
    apple = uncurry translate currentApplePosition $ applePic
      where
        applePic = scale 2 2 (imgs !! 1)
        currentApplePosition = appleLoc game

    -- Slime

    -- radius 25
    slimes = pictures [slime pos | pos <- (slimeLocs game)]

    slime position = uncurry translate position $ slimePic
      where
        slimePic = scale 0.14285 0.14285 (imgs !! 4)

    -- Score

    gameScore :: Picture
    gameScore = pictures [
      uncurry translate (440, 360) $ scale 0.2 0.2 $ color white $ text "Score: ",
      uncurry translate (440, 320) $ scale 0.2 0.2 $ color white $ text (show (playerScore game))
      ]

    -- High Score

    gameHighScore :: Picture
    gameHighScore = pictures [
      uncurry translate (440, 250) $ scale 0.2 0.2 $ color white $ text "High Score: ",
      uncurry translate (440, 210) $ scale 0.2 0.2 $ color white $ text (show (highScore game))
      ]

    -- Level
    gameLevel :: Picture
    gameLevel = pictures [
      uncurry translate (440, -230) $ scale 0.2 0.2 $ color white $ text "Level: ",
      uncurry translate (440, -270) $ scale 0.2 0.2 $ color white $ text (show (level game))
      ]

    -- Time alive

    timeDisplay :: Picture
    timeDisplay = pictures [
      uncurry translate (440, -340) $ scale 0.15 0.15 $ color white $ text ("Time alive:"),
      uncurry translate (440, -380) $ scale 0.15 0.15 $ color white $ text ((show (timeAlive game)) ++ " seconds")
      ]
    -- Bullet

    bulletPic :: Picture
    bulletPic = pictures [
      uncurry translate (fst (bullet game)) $ color black $ circleSolid (blockRadius / 1.4),
      uncurry translate (fst (bullet game)) $ color (light blue) $ circleSolid (blockRadius / 2)
      ]

    -- Walls

    wall :: Int -> Int -> Picture
    wall x y = translate offsetX offsetY $ wallColor $ shape
      where
        offsetX = if x == 1 then 0 else offset
        offsetY = if x == 0 then 0 else offset
        offset  = if y == 1 then windowRadius else (-windowRadius)
        shape   = if x == 1 then rectangleSolid windowSize blockSize else rectangleSolid blockSize windowSize
        wallColor = color $ greyN 0.5

    walls :: Picture
    walls = pictures [wall 1 1, wall 1 0, wall 0 0, wall 0 1]

    -- Grass

    grassPic = imgs !! 0
    gameGrass = pictures [uncurry translate (x, y) $ grassPic | x <- grassPositions, y <- grassPositions]
      where
        grassPositions = [-windowRadius + blockSize * 2, -windowRadius + blockSize * 2 + 80..windowRadius - blockSize * 2]

-- Fontes:
-- -> Para iniciar o projeto:
--      https://andrew.gibiansky.com/blog/haskell/haskell-gloss/
--        Nota: pulei a parte de colisões

-- -> Para carregar imagens:
--      https://blog.jayway.com/2020/11/01/making-a-small-game-with-gloss/

-- -> Colisão de círculos:
--      https://gamedevelopment.tutsplus.com/tutorials/when-worlds-collide-simulating-circle-circle-collisions--gamedev-769

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed
import System.Random
-- teste
toInt :: Float -> Int
toInt x = round x

blockSize, windowSize, snakeTailSpacing :: Float
blockSize = blockSizeConfig "normal" -- "normal" (default), "small" or "smaller"
windowSize = blockSize * 40.0
infoDisplayWidth = blockSize * 20.0
infoDisplayHeight = windowSize
snakeTailSpacing = tailSpacingConfig "none" -- "none" (default), "some" or "separate"
slimeGenerationRatio = 3 -- How many seconds between each slime generation

-- Helpers

windowRadius  = windowSize / 2
blockRadius   = blockSize / 2
defaultCircle = circleSolid blockRadius

--

blockSizeConfig :: [Char] -> Float
blockSizeConfig s
  | s == "normal" = 20.0 
  | s == "small" = 15.0
  | s == "smaller" = 10.0
  | otherwise = 20.0

tailSpacingConfig :: [Char] -> Float
tailSpacingConfig s
  | s == "none" = 5.0 * (blockSize / 20) 
  | s == "some" = 10.0 * (blockSize / 20)
  | s == "separate" = 20.0 * (blockSize / 20)
  | otherwise = 5.0

gameVelocityConfig :: [Char] -> Int
gameVelocityConfig v
  | v == "normal"   = 1
  | v == "hard"     = 2
  | v == "hell"     = 3
  | otherwise       = 1

truncate' :: Float -> Int -> Float
truncate' x n = (fromIntegral (floor (x * t))) / t
  where t = 10 ^ n

-- TODO Random positions based on windowSize (some configs doesnt work properly)
pseudoRandomNumber :: Float -> Float -> Float -> Float
pseudoRandomNumber seed r1 r2 = (take 1 . randomRs (r1, r2) . mkStdGen $ round seed) !! 0

pseudoRandomCoordinate :: Float -> Float -> (Float, Float)
pseudoRandomCoordinate x y = (pseudoRandomNumber x (-r) r, pseudoRandomNumber y (-r) r)
  where
    r = windowRadius - blockSize

width, height, offset :: Int
width = toInt windowSize + toInt infoDisplayWidth
height = toInt windowSize
offset = 100

window :: Display
window = InWindow "Snakell" (width, height) (offset, offset)

background :: Color
background = black

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
  } deriving Show

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

initialState :: SnakellGame
initialState = Game
  { snakeHeadLoc = initialHeadLoc
  , snakeTailLoc = initialSnakeTail
  , snakeDirection = initialSnakeDirection
  , appleLoc = (-(blockSize * 3), (blockSize * 3))
  , bullet = restingBulletState
  , playerScore = 0
  , timeAlive = 0
  , timePassed = 0
  , slimeLocs = [(100, 100), (0, 320), (-140, -140)]
  }

wallCollision :: Coordinate -> Float -> [Char] 
wallCollision (x, y) radius
  | y - radius <= -windowSize / 2 = "top" 
  | y + radius >=  windowSize / 2 = "bottom"
  | x - radius <= -windowSize / 2 = "left"
  | x + radius >=  windowSize / 2 = "right"
  | otherwise = "none"

circleCollision :: Coordinate -> Float -> Coordinate -> Float -> Bool
circleCollision (a1, a2) aRadius (b1, b2) bRadius = distance < aRadius + bRadius
  where
    x = a1 - b1;
    y = a2 - b2;
    distance = sqrt(x^2 + y^2);

render :: SnakellGame -> [Picture] -> Picture
render game imgs =
  pictures [gameGrass, apple, snakeTail, bulletPic, snakeHead, walls, gameScore, timeDisplay, slimes]

  where
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

snakeIncrement :: SnakeBlock -> [SnakeBlock]
snakeIncrement lastBlock = [lastBlock | t <- [1..snakeIncrementQuantity]]
  where
    snakeIncrementQuantity = blockSize / snakeTailSpacing

moveSnake :: Float -> SnakellGame -> SnakellGame
moveSnake seconds game = if snakeHitsTail then initialState else nextGameState
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
      }

    -- Localização e direção antiga

    (x, y) = snakeHeadLoc game
    (vx, vy) = snakeDirection game

    -- Novas localizações

    x' = x + vx * snakeTailSpacing
    y' = y + vy * snakeTailSpacing

    collision :: String
    collision = wallCollision (x', y') blockRadius
    (x'', y'') = newPosIfCollision collision (x', y')

    -- New score

    newScore :: Float
    newScore = (playerScore) game + scoreIncrement
      where
        scoreIncrement = if snakeEatsApple then 3 else 0

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


-- Respond to key events.
handleKeys :: Event -> SnakellGame -> SnakellGame

-- For an 'r' keypress, reset the snake's head to the center.
handleKeys (EventKey (Char 'r') _ _ _) game =
  game { snakeHeadLoc = (-180, 180) }

-- For an 's' keypress, shoot bullet
handleKeys (EventKey (Char 's') _ _ _) game =
  game { bullet = if bullet game /= restingBulletState then bullet game else (snakeHeadLoc game, snakeDirection game) }

handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game =
  game { snakeDirection = if snakeDirection game == (0, -1) then (0, -1) else (0, 1) }

handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game =
  game {  snakeDirection = if snakeDirection game == (0, 1) then (0, 1) else (0, -1) }

handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game =
  game { snakeDirection = if snakeDirection game == (-1, 0) then (-1, 0) else (1, 0) }

handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) game =
  game { snakeDirection = if snakeDirection game == (1, 0) then (1, 0) else (-1, 0) }

-- Do nothing for all other events.
handleKeys _ game = game

-- Numero de frames para mostrar por segundo
fps :: Int
fps = (gameVelocityConfig "normal") * 300 `div` (toInt snakeTailSpacing)

-- Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> SnakellGame -> SnakellGame
update seconds game = moveSnake seconds game

main :: IO ()
main = do
  applePicture <- loadBMP "src/apple.bmp"
  grassPicture <- loadBMP "src/grass.bmp"
  snakeHeadPicture <- loadBMP "src/snakeHead.bmp"
  snakeBlockPicture <- loadBMP "src/snakeBlock.bmp"
  slimePicture <- loadBMP "src/slime.bmp"

  play window background fps initialState (`render` [grassPicture, applePicture, snakeHeadPicture, snakeBlockPicture, slimePicture]) handleKeys update

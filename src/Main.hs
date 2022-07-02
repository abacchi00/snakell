-- Fontes:
-- https://andrew.gibiansky.com/blog/haskell/haskell-gloss/
--   Nota: pulei a parte de colisões

-- Colisão de círculos:
--   https://gamedevelopment.tutsplus.com/tutorials/when-worlds-collide-simulating-circle-circle-collisions--gamedev-769

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed
import System.Random

toInt :: Float -> Int
toInt x = round x

blockSize, windowSize, snakeTailSpacing :: Float
blockSize = blockSizeConfig "normal" -- "normal" (default), "small" or "smaller"
windowSize = blockSize * 40.0
snakeTailSpacing = tailSpacingConfig "none" -- "none" (default), "some" or "separate"

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
width = toInt windowSize
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
  } deriving Show

wallCollision :: Coordinate -> Float -> [Char] 
wallCollision (x, y) radius
  | y - radius <= -fromIntegral width / 2  = "top" 
  | y + radius >=  fromIntegral width / 2  = "bottom"
  | x - radius <= -fromIntegral height / 2 = "left"
  | x + radius >=  fromIntegral height / 2 = "right"
  | otherwise = "none"

appleCollision (a1, a2) (s1, s2) radius = distance < 2 * radius
  where
    a = a1 - s1;
    b = a2 - s2;
    distance = sqrt(((a1 - s1) * (a1 - s1)) + ((a2 - s2) * (a2 - s2)));

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
  }

render :: SnakellGame -> [Picture] -> Picture
render game imgs =
  pictures [gameGrass, apple, snakeTail, bulletPic, snakeHead, walls, gameScore]

  where
    -- Snake
    snakeHead :: Picture
    snakeHead = uncurry translate headPosition $ rotate headRotation (scale 0.08 0.08 snakeHeadPic)
      where
        headRotation
          | snakeDirection game == (0, -1) = 0
          | snakeDirection game == (0, 1)  = 180
          | snakeDirection game == (-1, 0) = 90
          | snakeDirection game == (1, 0)  = 270

    snakeHeadPic = imgs !! 2

    snakeTail :: Picture
    snakeTail = pictures [uncurry translate (fst tailBlock) $ snakeBlockPic | tailBlock <- snakeTailLoc game]

    snakeBlockPic = scale 0.04 0.04 (imgs !! 3)

    apple :: Picture
    apple = uncurry translate applePosition $ scale 2 2 applePic
      where
        applePic = imgs !! 1

    headPosition :: Coordinate
    headPosition = snakeHeadLoc game
    tailBlocks :: [SnakeBlock]
    tailBlocks = snakeTailLoc game
    applePosition :: Coordinate
    applePosition = appleLoc game

    -- Text

    gameScore = uncurry translate (-380, -380) $ scale 0.2 0.2 $ text ("Score: " ++ (show (playerScore game)))

    -- Bullet

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
    
    walls = pictures [wall 1 1, wall 1 0, wall 0 0, wall 0 1]

    -- Grass
    grassPic = imgs !! 0
    gameGrass = pictures [uncurry translate (x, y) $ grassPic | x <- grassPositions, y <- grassPositions]
      where
        grassPositions = [-windowRadius, -windowRadius + 80..windowRadius]
    
    -- Colors
    snakeHeadColor = color (dark (dark (dark green)))
    snakeTailColor = color (dark green)
    wallColor = color $ greyN 0.5

snakeIncrement :: SnakeBlock -> [SnakeBlock]
snakeIncrement lastBlock = [lastBlock | t <- [1..snakeIncrementQuantity]]
  where
    snakeIncrementQuantity = blockSize / snakeTailSpacing

moveSnake :: Float -> SnakellGame -> SnakellGame
moveSnake seconds game = game { snakeHeadLoc = (x'', y''), snakeTailLoc = newTailPositions (snakeTailLoc game), appleLoc = newAppleLoc, bullet = nextBulletState, playerScore = newScore }
  where
    -- Localização e direção antiga
    (x, y) = snakeHeadLoc game
    (vx, vy) = snakeDirection game

    -- Novas localizações
    x' = x + vx * snakeTailSpacing
    y' = y + vy * snakeTailSpacing
    -- x' = x + vx * seconds
    -- y' = y + vy * seconds

    collision = wallCollision (x', y') blockRadius
    (x'', y'') = newPosIfCollision collision (x', y')

    -- New score

    newScore :: Float
    newScore = (playerScore) game + scoreIncrement
      where
        scoreIncrement = if snakeEatsApple then 3 else 0

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

    snakeEatsApple :: Bool
    snakeEatsApple = appleCollision (appleLoc game) (x', y') blockRadius

    nextBulletState = if bulletWallCollision /= "none" then restingBulletState else moveBullet
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
  game { snakeDirection = (0, 1) }

handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game =
  game { snakeDirection = (0, -1) }

handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game =
  game { snakeDirection = (1, 0) }

handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) game =
  game { snakeDirection = (-1, 0) }

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

  play window background fps initialState (`render` [grassPicture, applePicture, snakeHeadPicture, snakeBlockPicture]) handleKeys update


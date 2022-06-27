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

-- TODO Random positions basend on windowSize (some configs doesnt work properly)
pseudoRandomNumber :: Float -> Float
pseudoRandomNumber seed = (take 1 . randomRs ((-19), 19) . mkStdGen $ round seed) !! 0

pseudoRandomCoordinate :: Float -> Float -> (Float, Float)
pseudoRandomCoordinate x y = (((pseudoRandomNumber x) `truncate'` 0) * 20, ((pseudoRandomNumber y) `truncate'` 0) * 20)

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

data SnakellGame = Game
  { snakeHeadLoc :: Coordinate -- Snake's head (x, y) location
  , snakeTailLoc :: [SnakeBlock] -- Snake's tail blocks (x, y) location
  , snakeDirection :: SnakeDirection -- Snake's directon: (0, -1) = down | (0, 1) = up | (-1, 0) = left | (1, 0) = right, 
  , appleLoc :: Coordinate -- Apple's (x, y) location
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
  }

render :: SnakellGame -> [Picture] -> Picture
render game imgs =
  pictures [apple, snakeTail, snakeHead, walls]

  where
    -- Snake
    snakeHead = uncurry translate headPosition $ color (dark (dark green)) $ circleSolid (blockSize / 2)

    snakeTail = pictures [uncurry translate (fst tailBlockPosition) $ head imgs | tailBlockPosition <- tailPositions]

    apple = uncurry translate (appleLoc game) $ color red $ rectangleSolid blockSize blockSize

    headPosition = snakeHeadLoc game
    tailPositions = snakeTailLoc game

    -- Walls
    horizontalWall, verticalWall :: Float -> Picture
    horizontalWall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid windowSize blockSize

    verticalWall offset =
      translate offset 0 $
        color wallColor $
          rectangleSolid blockSize windowSize

    walls = pictures [horizontalWall (windowSize / 2), horizontalWall (-(windowSize / 2)), verticalWall (windowSize / 2), verticalWall (-(windowSize / 2))]

    -- Colors
    snakeColor = green
    wallColor = greyN 0.5

snakeIncrement :: SnakeBlock -> [SnakeBlock]
snakeIncrement lastBlock = [lastBlock | t <- [1..snakeIncrementQuantity]]
  where
    snakeIncrementQuantity = blockSize / snakeTailSpacing

moveSnake :: Float -> SnakellGame -> SnakellGame
moveSnake seconds game = game { snakeHeadLoc = (x'', y''), snakeTailLoc = newTailPositions (snakeTailLoc game), appleLoc = newAppleLoc }
  where
    -- Localização e direção antiga
    (x, y) = snakeHeadLoc game
    (vx, vy) = snakeDirection game

    -- Novas localizações
    x' = x + vx * snakeTailSpacing
    y' = y + vy * snakeTailSpacing
    -- x' = x + vx * seconds
    -- y' = y + vy * seconds

    collision = wallCollision (x', y') (blockSize / 2)
    (x'', y'') = newPosIfCollision collision (x', y')

    teste :: SnakeBlock -> [SnakeBlock] -> [SnakeBlock]
    teste _ [] = []
    teste _ [a] = if (snakeEatsApple) then snakeIncrement a else []
    teste a (_:xs) = [a] ++ teste (head xs) xs

    newTailPositions :: [SnakeBlock] -> [SnakeBlock]
    newTailPositions xs = [((x, y), snakeDirection game)] ++ teste (head xs) xs

    newAppleLoc :: Coordinate
    newAppleLoc = if snakeEatsApple then genRandomAppleLoc x y else appleLoc game

    genRandomAppleLoc :: Float -> Float -> Coordinate
    genRandomAppleLoc x y = pseudoRandomCoordinate (x + 11) (y + 88)

    snakeEatsApple :: Bool
    snakeEatsApple = appleCollision (appleLoc game) (x', y') (blockSize / 2)

-- Respond to key events.
handleKeys :: Event -> SnakellGame -> SnakellGame

-- For an 'r' keypress, reset the snake's head to the center.
handleKeys (EventKey (Char 'r') _ _ _) game =
  game { snakeHeadLoc = (-180, 180) }

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
  snakeBlockPicture <- loadBMP "src/snakeBlock.bmp"

  play window background fps initialState (`render` [snakeBlockPicture]) handleKeys update


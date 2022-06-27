-- Fontes:
-- https://andrew.gibiansky.com/blog/haskell/haskell-gloss/
--   Nota: pulei a parte de colisões

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed
import System.Random

truncate' :: Float -> Int -> Float
truncate' x n = (fromIntegral (floor (x * t))) / t
  where t = 10 ^ n

pseudoRandomNumber :: Float -> Float
pseudoRandomNumber seed = (take 1 . randomRs ((-9), 9) . mkStdGen $ round seed) !! 0

pseudoRandomCoordinate :: Float -> Float -> (Float, Float)
pseudoRandomCoordinate x y = (((pseudoRandomNumber x) `truncate'` 0) * 20, ((pseudoRandomNumber y) `truncate'` 0) * 20)

width, height, offset :: Int
width = 400
height = 400
offset = 100

window :: Display
window = InWindow "Snakell" (width, height) (offset, offset)

background :: Color
background = black

data SnakellGame = Game
  { snakeHeadLoc :: (Float, Float) -- Snake's head (x, y) location
  , snakeTailLoc :: [(Float, Float)] -- Snake's tail blocks (x, y) location
  , snakeVel :: (Float, Float) -- Pixels por frame, como os blocos do jogo são 20 x 20, a cobra anda de 20 em 20 pixels
  , appleLoc :: (Float, Float) -- Apple's (x, y) location
  } deriving Show

initialState :: SnakellGame
initialState = Game
  { snakeHeadLoc = (100, 0)
  , snakeTailLoc = [(100, 20), (100, 40), (100, 60)]
  , snakeVel = (0, -20) -- (0, -20) é pra baixo, (0, 20) é pra cima, (-20, 0) é pra esquerda, (20, 0) é pra direita, 
  , appleLoc = (-60, 60)
  }

render :: SnakellGame -> Picture
render game =
  pictures [walls, apple, snakeHead, snakeTail]

  where
    -- Snake
    snakeHead = uncurry translate headPosition $ color (dark (dark green)) $ rectangleSolid 20 20

    snakeTail = pictures [uncurry translate tailBlockPosition $ color snakeColor $ circleSolid 10 | tailBlockPosition <- tailPositions ]

    apple = uncurry translate (appleLoc game) $ color red $ rectangleSolid 20 20

    headPosition = snakeHeadLoc game
    tailPositions = snakeTailLoc game
    

    -- Walls
    horizontalWall, verticalWall :: Float -> Picture
    horizontalWall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 400 20

    verticalWall offset =
      translate offset 0 $
        color wallColor $
          rectangleSolid 20 400

    walls = pictures [horizontalWall 200, horizontalWall (-200), verticalWall 200, verticalWall (-200)]

    -- Colors
    snakeColor = green
    wallColor = greyN 0.5

moveSnake :: Float -> SnakellGame -> SnakellGame
moveSnake seconds game = game { snakeHeadLoc = (x', y'), snakeTailLoc = newTailPositions (snakeTailLoc game), appleLoc = newAppleLoc }
  where
    -- Localização e velocidade antiga
    (x, y) = snakeHeadLoc game
    (vx, vy) = snakeVel game

    -- Novas localizações
    x' = x + vx
    y' = y + vy
    -- x' = x + vx * seconds
    -- y' = y + vy * seconds

    teste _ [] = []
    teste _ [a] = if (snakeEatsApple) then [a] else []
    teste a (_:xs) = [a] ++ teste (head xs) xs
    newTailPositions xs = [(x, y)] ++ teste (head xs) xs

    newAppleLoc :: (Float, Float)
    newAppleLoc = if snakeEatsApple then pseudoRandomCoordinate (x + 79) (y + 32) else appleLoc game

    snakeEatsApple = snakeHeadLoc game == appleLoc game

-- | Respond to key events.
handleKeys :: Event -> SnakellGame -> SnakellGame

-- For an 'r' keypress, reset the snake's head to the center.
handleKeys (EventKey (Char 'r') _ _ _) game =
  game { snakeHeadLoc = (-180, 180) }

handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game =
  game { snakeVel = (0, 20) }

handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game =
  game { snakeVel = (0, -20) }

handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game =
  game { snakeVel = (20, 0) }

handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) game =
  game { snakeVel = (-20, 0) }

-- Do nothing for all other events.
handleKeys _ game = game


-- Numero de frames para mostrar por segundo
fps :: Int
-- fps = 60
fps = 10

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> SnakellGame -> SnakellGame
update seconds game = moveSnake seconds game

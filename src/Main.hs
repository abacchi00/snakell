-- Fontes:
-- https://andrew.gibiansky.com/blog/haskell/haskell-gloss/
--   Nota: pulei a parte de colisões

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

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
  , snakeVel :: (Float, Float) -- Pixels por frame, como os blocos do jogo são 20 x 20, a cobra anda de 20 em 20 pixels
  } deriving Show

initialState :: SnakellGame
initialState = Game
  { snakeHeadLoc = (100, 100)
  , snakeVel = (0, -20) -- (0, -20) é pra baixo, (0, 20) é pra cima, (-20, 0) é pra esquerda, (20, 0) é pra direita, 
  }

render :: SnakellGame -> Picture
render game =
  pictures [snakeHead, snakeTail, walls]

  where
    -- Snake
    snakeHead = uncurry translate headPosition $ color snakeColor $ rectangleSolid 20 20

    snakeTail = uncurry translate tailPosition $ color blue $ rectangleSolid 20 20

    tailPosition = (fst headPosition - fst vel, snd headPosition - snd vel)
    headPosition = snakeHeadLoc game
    vel = snakeVel game

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
moveSnake seconds game = game { snakeHeadLoc = (x', y') }
  where
    -- Localização e velocidade antiga
    (x, y) = snakeHeadLoc game
    (vx, vy) = snakeVel game

    -- Novas localizações
    x' = x + vx
    y' = y + vy
    -- x' = x + vx * seconds
    -- y' = y + vy * seconds


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
fps = 5

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> SnakellGame -> SnakellGame
update seconds game = moveSnake seconds game

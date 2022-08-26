-- Fontes:
-- -> Para iniciar o projeto:
--      https://andrew.gibiansky.com/blog/haskell/haskell-gloss/
--        Nota: pulei a parte de colisões

-- -> Para carregar imagens:
--      https://blog.jayway.com/2020/11/01/making-a-small-game-with-gloss/

-- -> Colisão de círculos:
--      https://gamedevelopment.tutsplus.com/tutorials/when-worlds-collide-simulating-circle-circle-collisions--gamedev-769

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Helpers
import Types
import Render
import Movement
import Base
import Keys


-- Number of frames per second
fps :: Int
fps = (gameVelocityConfig "normal") * 300 `div` (toInt snakeTailSpacing)

-- Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> SnakellGame -> SnakellGame
update seconds game = moveSnake seconds game

main :: IO ()
main = do
  applePicture <- loadBMP "assets/apple.bmp"
  grassPicture <- loadBMP "assets/grass.bmp"
  snakeHeadPicture <- loadBMP "assets/snakeHead.bmp"
  snakeBlockPicture <- loadBMP "assets/snakeBlock.bmp"
  slimePicture <- loadBMP "assets/slime.bmp"
  blockScreen <- loadBMP "assets/blockScreen.bmp"

  play window background fps (initialState 0.0) (`render` [grassPicture, applePicture, snakeHeadPicture, snakeBlockPicture, slimePicture, blockScreen]) handleKeys update

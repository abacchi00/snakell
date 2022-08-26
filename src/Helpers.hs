module Helpers where

import Graphics.Gloss
import Data.Fixed
import System.Random
import Types


-- teste
toInt :: Float -> Int
toInt x = round x

blockSize, windowSize, snakeTailSpacing :: Float
blockSize = blockSizeConfig "normal" -- "normal" (default), "small" or "smaller"
windowSize = blockSize * 40.0
infoDisplayWidth = blockSize * 20.0
infoDisplayHeight = windowSize
snakeTailSpacing = tailSpacingConfig "none" -- "none" (default), "some" or "separate"
slimeGenerationRatio = 3 :: Int -- How many seconds between each slime generation

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

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

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

snakeIncrement :: SnakeBlock -> [SnakeBlock]
snakeIncrement lastBlock = [lastBlock | t <- [1..snakeIncrementQuantity]]
  where
    snakeIncrementQuantity = blockSize / snakeTailSpacing
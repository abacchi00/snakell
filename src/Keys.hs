module Keys where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Types
import Base

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

handleKeys (EventKey (Char 'a') _ _ _) game = 
  game { gameRunning = True }

-- Do nothing for all other events.
handleKeys _ game = game
module InGame (gameDisplay) where

import Graphics.Gloss
import Types (CannonType(..), ScenarioType (..), Jugador(..))

drawCannon :: Jugador -> Float -> Float -> Picture
drawCannon jugador x y = translate x y (scale 2 2 (sprite jugador)) --con scale 2 2 agrandamos la imagen un 200%

-- Se dibuja la línea divisoria en el centro de la pantalla
drawDivider :: Picture
drawDivider = translate 0 0 (color white (line [(0, -600), (0, 600)]))

-- Línea horizontal bajo los cañones
drawDivider2 :: Picture
drawDivider2 = translate 0 (-280) (color white (line [(-600, 0), (600, 0)]))

gameDisplay :: Jugador -> Jugador -> Picture -> Picture
gameDisplay p1Cannon p2Cannon escenario = 
  pictures [
      escenario,
      drawCannon p1Cannon (-350) (-250),  -- Dibuja el sprite de Player1
      drawCannon p2Cannon 350 (-250),     -- Dibuja el sprite de Player2
      drawDivider,
      drawDivider2
  ]
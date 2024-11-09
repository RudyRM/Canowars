module InGame (gameDisplay) where

import Disparo
import Data.Maybe (isNothing, fromJust)
import Graphics.Gloss
import Types (CannonType(..), ScenarioType (..), Jugador(..), Proyectil(..))

drawCannon :: Jugador -> Float -> Float -> Picture
drawCannon jugador x y = translate x y (scale 2 2 (sprite jugador)) --con scale 2 2 agrandamos la imagen un 200%

-- Se dibuja la línea divisoria en el centro de la pantalla
drawDivider :: Picture
drawDivider = translate 0 0 (color white (line [(0, -600), (0, 600)]))

-- Línea horizontal bajo los cañones
drawDivider2 :: Picture
drawDivider2 = translate 0 (-280) (color white (line [(-600, 0), (600, 0)]))

-- Función que dibuja un punto individual
drawPoint :: (Float, Float) -> Picture
drawPoint (x, y) = Translate x y (Color white (Circle 3))  -- Usamos un círculo pequeño como punto

gameDisplay :: Jugador -> Jugador -> Picture -> Picture
gameDisplay p1Cannon p2Cannon escenario =
  pictures [
    Translate 0 0 (Scale 0.675 0.675 escenario),
    drawCannon p1Cannon (-350) (-250),
    drawCannon p2Cannon 350 (-250),
    pictures (map drawPoint (parabolaPoints (-350) (angulo p1Cannon))),
    drawDivider,
    drawDivider2
  ]
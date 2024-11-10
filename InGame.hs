module InGame (gameDisplay) where

import Disparo
import Data.Maybe (isNothing, fromJust)
import Graphics.Gloss
import Types (CannonType(..), ScenarioType (..), Jugador(..), Proyectil(..))

drawCannon :: Jugador -> Float -> Picture
drawCannon jugador y = translate (posX jugador) y (scale 2 2 (sprite jugador))

-- Se dibuja la línea divisoria en el centro de la pantalla
drawDivider :: Picture
drawDivider = translate 0 0 (color white (line [(0, -600), (0, 600)]))

-- Línea horizontal bajo los cañones
drawDivider2 :: Picture
drawDivider2 = translate 0 (-280) (color white (line [(-600, 0), (600, 0)]))

drawFuel :: Jugador -> Float -> Picture
drawFuel jugador x =
  let fuelPercentage = (fromIntegral (combustible jugador) / 100) * 100
      fuelText = "Fuel: " ++ show (round fuelPercentage) ++ "%"
  in translate x 320 (scale 0.3 0.3 (color white (text fuelText)))

-- Función que dibuja un punto individual
drawPoint :: (Float, Float) -> Picture
drawPoint (x, y) = Translate x y (Color white (Circle 3))  -- Usamos un círculo pequeño como punto

gameDisplay :: Jugador -> Jugador -> Picture -> Picture
gameDisplay p1Cannon p2Cannon escenario =
  pictures [
    Translate 0 0 (Scale 0.675 0.675 escenario),
    drawCannon p1Cannon (-250),
    drawCannon p2Cannon (-250),
    -- Dibujar la parábola del tanque 1
    pictures (map drawPoint (parabolaPoints1 (posX p1Cannon) (-250) (angulo p1Cannon))),
    -- Dibujar la parábola del tanque 2
    pictures (map drawPoint (parabolaPoints2 (posX p2Cannon) (-250) (angulo p2Cannon))),
    drawDivider,
    drawDivider2,
    drawFuel p1Cannon (-580),   -- Combustible del jugador 1 en la esquina superior izquierda
    drawFuel p2Cannon 480       -- Combustible del jugador 2 en la esquina superior derecha
  ]

module InGame (gameDisplay) where

import Disparo
import Data.Maybe (isNothing, fromJust)
import Graphics.Gloss
import Types (CannonType(..), ScenarioType (..), Jugador(..), Proyectil(..), Turno(..))

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

gameDisplay :: Turno -> Jugador -> Jugador -> Picture -> Maybe Proyectil -> Picture
gameDisplay turno p1Cannon p2Cannon escenario proyectil =
  let selectCanon = if turno == Jugador1 then p1Cannon else p2Cannon
  in case proyectil of
    Just bala -> pictures [
               Translate 0 0 (Scale 0.675 0.675 escenario),
               drawCannon p1Cannon (posX p1Cannon) (-250),
               drawCannon p2Cannon (posX p2Cannon) (-250),
               -- Dibuja la bala en su nueva posición actualizada
               Translate (posXProyectil bala) (posYProyectil bala) (Scale 0.3 0.3 (spriteProyectil bala)),
               -- Puntos de la parábola para mostrar trayectoria (opcional)
                pictures (map drawPoint (parabolaPoints (posX selectCanon) (-250) (angulo selectCanon) (if turno == Jugador1 then 1 else (-1)))),
               drawDivider,
               drawDivider2
             ]
    Nothing -> pictures [
                Translate 0 0 (Scale 0.675 0.675 escenario),
                drawCannon p1Cannon (posX p1Cannon) (-250),
                drawCannon p2Cannon (posX p2Cannon) (-250),
                -- Traza la parábola inicial sin proyectil
                pictures (map drawPoint (parabolaPoints (posX selectCanon) (-250) (angulo selectCanon) (if turno == Jugador1 then 1 else (-1)))),
                drawDivider,
                drawDivider2
              ]

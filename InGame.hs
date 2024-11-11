module InGame (gameDisplay) where

import Disparo
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (isNothing, fromJust)
import Graphics.Gloss
import Tipos (Jugador(..), Proyectil(..), Turno(..))
import Imagenes (comb1, comb2, comb3, comb4, comb5)

drawCannon :: Turno -> Jugador -> Float -> Float -> Picture
drawCannon Jugador1 jugador x y = translate x y (scale 2 2 (spriteJugador jugador)) --con scale 2 2 agrandamos la imagen un 200%
drawCannon Jugador2 jugador x y = translate x y (scale (-2) 2 (spriteJugador jugador)) --con scale 2 2 agrandamos la imagen un 200%

-- Línea horizontal bajo los cañones
drawDivider :: Picture
drawDivider = Translate 0 (-125) (Scale 0.5 0.5 pared)

pared :: Picture
pared = unsafePerformIO $ loadBMP "assets/fondos/pared.bmp"

drawFuel :: Jugador -> Float -> Picture
drawFuel jugador x =
  let fuelPercentage = (fromIntegral (combustibleJugador jugador) / 100) * 100
      fuelText = "Fuel: " ++ show (round fuelPercentage) ++ "%"
  in translate x 320 (scale 0.3 0.3 (color black (text fuelText)))

drawHP:: Jugador -> Float -> Picture
drawHP jugador x =
  let hpPercentage = (fromIntegral (vida jugador) / 100) * 100
      hptext = "HP: " ++ show (round hpPercentage) ++ "%"
  in translate x 320 (scale 0.3 0.3 (color white (text hptext)))

-- Función que dibuja un punto individual
drawPoint :: (Float, Float) -> Picture
drawPoint (x, y) = Translate x y (Color white (Circle 3))  -- Usamos un círculo pequeño como punto

gameDisplay :: Turno -> Jugador -> Jugador -> Picture -> Maybe Proyectil -> Picture
gameDisplay turno p1Cannon p2Cannon escenario proyectil =
  let
    selectCanon = if turno == Jugador1 then p1Cannon else p2Cannon
    combustibleP1 = combustibleJugador p1Cannon
    imgCombustibleP1
      | combustibleP1 <= 40 = comb5
      | combustibleP1 <= 80 = comb4
      | combustibleP1 <= 120 = comb3
      | combustibleP1 <= 160 = comb2
      | combustibleP1 <= 200 = comb1

    combustibleP2 = combustibleJugador p2Cannon
    imgCombustibleP2
      | combustibleP2 <= 40 = comb5
      | combustibleP2 <= 80 = comb4
      | combustibleP2 <= 120 = comb3
      | combustibleP2 <= 160 = comb2
      | combustibleP2 <= 200 = comb1
      
    -- Configuración inicial común
    baseElements = [
      Translate 0 0 (Scale 0.675 0.675 escenario),
      drawCannon Jugador1 p1Cannon (posXJugador p1Cannon) (-250),
      drawCannon Jugador2 p2Cannon (posXJugador p2Cannon) (-250),
      -- Traza la parábola inicial o actual
      pictures (map drawPoint (puntosParabola (posXJugador selectCanon) (-250) (anguloJugador selectCanon) (if turno == Jugador1 then 1 else (-1)))),
      drawDivider,
      Translate (-500) 250 (Scale 4 4 imgCombustibleP1),
      Translate (500) 250 (Scale 4 4 imgCombustibleP2),
      drawHP p1Cannon (-300),
      drawHP p2Cannon (150)]
    -- Agrega el proyectil solo si existe
    proyectilElement = case proyectil of
      Just bala -> [Translate (posXProyectil bala) (posYProyectil bala) (Scale 0.3 0.3 (spriteProyectil bala))]
      Nothing   -> []
  in
    pictures (baseElements ++ proyectilElement)


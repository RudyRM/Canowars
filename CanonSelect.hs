module CanonSelect (drawCanonSelectionScreen, drawSelectionSummary) where

import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)
import Types (Player(..), CannonType(..)) -- Importa Player y CannonType

drawCanonSelectionScreen :: Player -> Picture
drawCanonSelectionScreen player = Pictures
  [ 
    Translate 0 0 (Scale 0.675 0.675 fondo)
  , Translate 0 250 (Scale 0.55 0.55 seleccion)
  , Translate (-200) 50 (Scale 3 3 tanque_nazi)
  , Translate 200 50 (Scale 3 3 tanque_comunista)
  , Translate (-200) (-200) (Scale 3 3 tanque_vaticano)
  , Translate 200 (-200) (Scale 3 3 tanque_eeuu)
  , Translate (-205) (-30) (Scale 0.275 0.275 num_1)
  , Translate 195 (-30) (Scale 0.275 0.275 num_2)
  , Translate (-205) (-280) (Scale 0.275 0.275 num_3)
  , Translate 195 (-280) (Scale 0.275 0.275 num_4)
  ]

drawSelectionSummary :: CannonType -> CannonType -> Picture
drawSelectionSummary p1Cannon p2Cannon = Pictures
  [ Translate (-300) 100 (Scale 0.5 0.5 (Text "Resumen de Seleccion"))
  , Translate (-300) 50 (Scale 0.4 0.4 (Text $ "Jugador 1 selecciono: " ++ show p1Cannon))
  , Translate (-300) 0 (Scale 0.4 0.4 (Text $ "Jugador 2 selecciono: " ++ show p2Cannon))
  , Translate (-300) (-50) (Scale 0.3 0.3 (Text "Presiona 'q' para volver al menu"))
  ]

fondo :: Picture
fondo = unsafePerformIO $ loadBMP "assets/fondos/War2/Pale/War2.bmp"

seleccion :: Picture
seleccion = unsafePerformIO $ loadBMP "assets/seleccion.bmp"

tanque_nazi :: Picture
tanque_nazi = unsafePerformIO $ loadBMP "assets/tanques/tank_nazi.bmp"
tanque_comunista :: Picture
tanque_comunista = unsafePerformIO $ loadBMP "assets/tanques/tank_comunista.bmp"
tanque_vaticano :: Picture
tanque_vaticano = unsafePerformIO $ loadBMP "assets/tanques/tank_vaticano.bmp"
tanque_eeuu :: Picture
tanque_eeuu = unsafePerformIO $ loadBMP "assets/tanques/tank_eeuu.bmp"

num_1 :: Picture
num_1 = unsafePerformIO $ loadBMP "assets/1.bmp"
num_2 :: Picture
num_2 = unsafePerformIO $ loadBMP "assets/2.bmp"
num_3 :: Picture
num_3 = unsafePerformIO $ loadBMP "assets/3.bmp"
num_4 :: Picture
num_4 = unsafePerformIO $ loadBMP "assets/4.bmp"

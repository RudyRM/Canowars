module PantallaFinal (dibujarPantallaFinal) where

import Graphics.Gloss (Picture (Pictures, Translate, Scale), loadBMP)
import System.IO.Unsafe (unsafePerformIO)
import Tipos (Jugador(..))

dibujarPantallaFinal :: Jugador ->  Picture
dibujarPantallaFinal tanqueGanador = Pictures
  [ 
    Translate 0 0 (Scale 0.675 0.675 fondo)
  , Translate (0) 250 (Scale 1 1 texto)
  , Translate 0 (30) (Scale 4 4 (spriteJugador tanqueGanador))
  , Translate 0 (-100) (Scale 0.55 0.55 (textoJugador tanqueGanador))
  , Translate (0) (-250) (Scale 0.6 0.6 volver)
  ]

fondo :: Picture
fondo = unsafePerformIO $ loadBMP "assets/fondos/mapa4_2.bmp"

texto :: Picture
texto = unsafePerformIO $ loadBMP "assets/ui/ganador.bmp"

volver :: Picture
volver = unsafePerformIO $ loadBMP "assets/ui/volver.bmp"

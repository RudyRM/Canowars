module SeleccionEscenario (dibujarSeleccionDeEscenario) where

import Imagenes (mapa1, mapa2, mapa3, mapa4, num1, num2, num3, num4)
import Graphics.Gloss (Picture (Pictures, Translate, Scale), loadBMP)
import System.IO.Unsafe (unsafePerformIO)
import Tipos (Turno(..))

-- Dibuja la pantalla de selección de escenarios
dibujarSeleccionDeEscenario :: Picture
dibujarSeleccionDeEscenario = Pictures
  [ 
    Translate 0 0 (Scale 0.675 0.675 fondo)
  , Translate (-200) 50 (Scale 0.1 0.1 mapa1)
  , Translate 200 50 (Scale 0.1 0.1 mapa2)
  , Translate (-200) (-200) (Scale 0.1 0.1 mapa3)
  , Translate 200 (-200) (Scale 0.1 0.1 mapa4)
  , Translate (-205) (-30) (Scale 0.275 0.275 num1)
  , Translate 195 (-30) (Scale 0.275 0.275 num2)
  , Translate (-205) (-280) (Scale 0.275 0.275 num3)
  , Translate 195 (-280) (Scale 0.275 0.275 num4)
  ]

fondo :: Picture
fondo = unsafePerformIO $ loadBMP "assets/fondos/mapa3_2.bmp"

module SeleccionCanon (dibujarSeleccionDeCanon) where

import Imagenes (tanque1, tanque2, tanque3, tanque4, num1, num2, num3, num4)
import Graphics.Gloss (Picture (Pictures, Translate, Scale), loadBMP)
import System.IO.Unsafe (unsafePerformIO)

dibujarSeleccionDeCanon :: Picture
dibujarSeleccionDeCanon = Pictures
  [ 
    Translate 0 0 (Scale 0.675 0.675 fondo)
  , Translate 0 250 (Scale 0.55 0.55 seleccion)
  , Translate (-200) 50 (Scale 3 3 tanque1)
  , Translate 200 50 (Scale 3 3 tanque2)
  , Translate (-200) (-200) (Scale 3 3 tanque3)
  , Translate 200 (-200) (Scale 3 3 tanque4)
  , Translate (-205) (-30) (Scale 0.275 0.275 num1)
  , Translate 195 (-30) (Scale 0.275 0.275 num2)
  , Translate (-205) (-280) (Scale 0.275 0.275 num3)
  , Translate 195 (-280) (Scale 0.275 0.275 num4)
  ]

fondo :: Picture
fondo = unsafePerformIO $ loadBMP "assets/fondos/mapa2_2.bmp"

seleccion :: Picture
seleccion = unsafePerformIO $ loadBMP "assets/ui/seleccion.bmp"


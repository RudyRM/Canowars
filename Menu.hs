module Menu (dibujarMenu) where

import Graphics.Gloss (Picture (Scale, Translate, Pictures), loadBMP)
import System.IO.Unsafe (unsafePerformIO)

dibujarMenu :: Picture
dibujarMenu = Pictures
  [ 
    Translate 0 0 (Scale 0.675 0.675 fondoMenu),
    Translate 0 (50) (Scale 0.9 0.9 logoCanowars),
    Translate 0 (-75) (Scale 0.25 0.25 texto)
  ]

fondoMenu :: Picture
fondoMenu = unsafePerformIO $ loadBMP "assets/fondos/mapa1_2.bmp"

logoCanowars :: Picture
logoCanowars = unsafePerformIO $ loadBMP "assets/ui/logo.bmp"

texto :: Picture
texto = unsafePerformIO $ loadBMP "assets/ui/pulsa.bmp"

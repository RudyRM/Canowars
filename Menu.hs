module Menu (drawMenu) where

import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)

drawMenu :: Picture
drawMenu = Pictures
  [ 
    Translate 0 0 (Scale 0.675 0.675 fondo),
    Translate 0 (50) (Scale 0.9 0.9 logo),
    Translate 0 (-75) (Scale 0.25 0.25 texto)
  ]

fondo :: Picture
fondo = unsafePerformIO $ loadBMP "assets/fondos/War1/Pale/War.bmp"

logo :: Picture
logo = unsafePerformIO $ loadBMP "assets/logo.bmp"

texto :: Picture
texto = unsafePerformIO $ loadBMP "assets/pulsa.bmp"

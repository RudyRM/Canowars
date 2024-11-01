module Menu (drawMenu) where

import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)

drawMenu :: Picture
drawMenu = Pictures
  [ Translate (-300) 0 (Scale 0.5 0.5 (Text "Presiona Enter para continuar"))
  , image
  ]

image :: Picture
image = unsafePerformIO $ loadBMP "assets/menu.bmp"  -- Carga la imagen desde assets

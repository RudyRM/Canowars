module Imagenes where

import Graphics.Gloss (Picture, loadBMP)
import System.IO.Unsafe (unsafePerformIO)

tanque1 :: Picture
tanque1 = unsafePerformIO $ loadBMP "assets/tanques/tanque1.bmp"
tanque2 :: Picture
tanque2 = unsafePerformIO $ loadBMP "assets/tanques/tanque2.bmp"
tanque3 :: Picture
tanque3 = unsafePerformIO $ loadBMP "assets/tanques/tanque3.bmp"
tanque4 :: Picture
tanque4 = unsafePerformIO $ loadBMP "assets/tanques/tanque4.bmp"

num1 :: Picture
num1 = unsafePerformIO $ loadBMP "assets/ui/num1.bmp"
num2 :: Picture
num2 = unsafePerformIO $ loadBMP "assets/ui/num2.bmp"
num3 :: Picture
num3 = unsafePerformIO $ loadBMP "assets/ui/num3.bmp"
num4 :: Picture
num4 = unsafePerformIO $ loadBMP "assets/ui/num4.bmp"

mapa1 :: Picture
mapa1 = unsafePerformIO $ loadBMP "assets/fondos/mapa1_1.bmp"
mapa2 :: Picture
mapa2 = unsafePerformIO $ loadBMP "assets/fondos/mapa2_1.bmp"
mapa3 :: Picture
mapa3 = unsafePerformIO $ loadBMP "assets/fondos/mapa3_1.bmp"
mapa4 :: Picture
mapa4 = unsafePerformIO $ loadBMP "assets/fondos/mapa4_1.bmp"

imgProyectil :: Picture
imgProyectil = (unsafePerformIO $ loadBMP "assets/tanques/proyectil.bmp")

imgProyectilCritico :: Picture
imgProyectilCritico = (unsafePerformIO $ loadBMP "assets/tanques/proyectil_critico.bmp")

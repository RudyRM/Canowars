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

vidp1_1 :: Picture
vidp1_1 = unsafePerformIO $ loadBMP "assets/ui/vidp1_1.bmp"
vidp1_2 :: Picture
vidp1_2 = unsafePerformIO $ loadBMP "assets/ui/vidp1_2.bmp"
vidp1_3 :: Picture
vidp1_3 = unsafePerformIO $ loadBMP "assets/ui/vidp1_3.bmp"
vidp1_4 :: Picture
vidp1_4 = unsafePerformIO $ loadBMP "assets/ui/vidp1_4.bmp"

vidp2_1 :: Picture
vidp2_1 = unsafePerformIO $ loadBMP "assets/ui/vidp2_1.bmp"
vidp2_2 :: Picture
vidp2_2 = unsafePerformIO $ loadBMP "assets/ui/vidp2_2.bmp"
vidp2_3 :: Picture
vidp2_3 = unsafePerformIO $ loadBMP "assets/ui/vidp2_3.bmp"
vidp2_4 :: Picture
vidp2_4 = unsafePerformIO $ loadBMP "assets/ui/vidp2_4.bmp"

comb1 :: Picture
comb1 = unsafePerformIO $ loadBMP "assets/ui/comb1.bmp"
comb2 :: Picture
comb2 = unsafePerformIO $ loadBMP "assets/ui/comb2.bmp"
comb3 :: Picture
comb3 = unsafePerformIO $ loadBMP "assets/ui/comb3.bmp"
comb4 :: Picture
comb4 = unsafePerformIO $ loadBMP "assets/ui/comb4.bmp"
comb5 :: Picture
comb5 = unsafePerformIO $ loadBMP "assets/ui/comb5.bmp"

mapa1 :: Picture
mapa1 = unsafePerformIO $ loadBMP "assets/fondos/mapa1_1.bmp"
mapa2 :: Picture
mapa2 = unsafePerformIO $ loadBMP "assets/fondos/mapa2_1.bmp"
mapa3 :: Picture
mapa3 = unsafePerformIO $ loadBMP "assets/fondos/mapa3_1.bmp"
mapa4 :: Picture
mapa4 = unsafePerformIO $ loadBMP "assets/fondos/mapa4_1.bmp"

jugador1 :: Picture
jugador1 = unsafePerformIO $ loadBMP "assets/ui/jugador1.bmp"
jugador2 :: Picture
jugador2 = unsafePerformIO $ loadBMP "assets/ui/jugador2.bmp"

imgProyectil :: Picture
imgProyectil = (unsafePerformIO $ loadBMP "assets/tanques/proyectil.bmp")

imgProyectilCritico :: Picture
imgProyectilCritico = (unsafePerformIO $ loadBMP "assets/tanques/proyectil_critico.bmp")

import Graphics.Gloss

-- Definición de Punto2D
data Punto2D a = Punto2D a a deriving Show

-- Instancia Functor para Punto2D
instance Functor Punto2D where
    fmap f (Punto2D x y) = Punto2D (f x) (f y)

-- Función para calcular la parábola
parabola :: Float -> Float -> Float -> Float -> Float
parabola x angulo posX posY = posY + (x - posX) * (x - posX) * tan angulo

-- Generación de los puntos de la parábola
puntosParabola :: Float -> Float -> Float -> Float -> [Punto2D Float]
puntosParabola posX posY anguloInicial lado =
    [Punto2D x (parabola x anguloInicial posX posY) | x <- [posX, posX + (10 * lado) .. posX + (300 * lado)]]

-- Valores de ejemplo
scaleFactor :: Float
scaleFactor = 0.1  -- Factor de escala para ajustar los puntos a la pantalla.

posXJugador :: Float -> Float
posXJugador _ = 0  -- El jugador comienza en X = 0.

anguloJugador :: Float -> Float
anguloJugador _ = 0.1  -- Ángulo de la parábola en radianes.

ladoJugador :: Float -> Float
ladoJugador _ = 1  -- Tamaño del paso (largo del lado).

selectCanon :: Float
selectCanon = 0  -- El jugador selecciona el cañón en la posición 0 (para simplificar).

-- Función para dibujar los puntos
drawPoint :: Punto2D Float -> Picture
drawPoint (Punto2D x y) = Translate x y (Color white (Circle 3))

-- Puntos de la parábola mapeados
puntosParabolaMapeados :: [Punto2D Float]
puntosParabolaMapeados = map (fmap (* scaleFactor)) (puntosParabola (posXJugador selectCanon) (-250) (anguloJugador selectCanon) (ladoJugador selectCanon))

-- Función principal para mostrar los puntos en la pantalla
main :: IO ()
main = display (InWindow "Parabola" (600, 600) (10, 10)) black picturesP
  where
    picturesP = pictures (map drawPoint puntosParabolaMapeados)

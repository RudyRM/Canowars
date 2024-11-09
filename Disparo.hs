module Disparo where
import Graphics.Gloss

-- Parámetros de lanzamiento
anguloInicial :: Float  -- Ángulo de lanzamiento en radianes
anguloInicial = pi / 4  -- Por ejemplo, 45 grados

velocidadInicial :: Float
velocidadInicial = 100  -- Puedes ajustar la velocidad según lo necesites

-- Punto inicial de la parábola
puntoInicialX :: Float
puntoInicialX = -350

puntoInicialY :: Float
puntoInicialY = -250

-- Función de la parábola que depende del ángulo inicial y la velocidad
parabola :: Float -> Float -> Float
parabola x anguloInicial = puntoInicialY + tan anguloInicial * (x - puntoInicialX) - (gravedad / (2 * (velocidadInicial * cos anguloInicial) ^ 2)) * (x - puntoInicialX) ^ 2
  where
    gravedad = 9.8

-- Genera puntos de la parábola a partir de la posición inicial
parabolaPoints :: Float -> Float -> [(Float, Float)]
parabolaPoints posX anguloInicial = [(x, (parabola x anguloInicial)) | x <- [posX, posX + 10 .. posX + 300]]
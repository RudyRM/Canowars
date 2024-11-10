module Disparo where
import Graphics.Gloss

-- Parámetros de lanzamiento
velocidadInicial :: Float
velocidadInicial = 100  -- Puedes ajustar la velocidad según lo necesites

-- Función de la parábola que depende del ángulo inicial y la velocidad
parabola :: Float -> Float -> Float -> Float -> Float
parabola x posX posY anguloInicial = 
    posY + tan anguloInicial * (x - posX) - (gravedad / (2 * (velocidadInicial * cos anguloInicial) ^ 2)) * (x - posX) ^ 2
  where
    gravedad = 9.8

-- Generar puntos para la parábola para el tanque 1 (dispara hacia la derecha)
parabolaPoints1 :: Float -> Float -> Float -> [(Float, Float)]
parabolaPoints1 posX posY anguloInicial =
    [(x, parabola x posX posY anguloInicial) | x <- [posX, posX + 10 .. posX + 300]]

-- Generar puntos para la parábola para el tanque 2 (dispara hacia la izquierda)
parabolaPoints2 :: Float -> Float -> Float -> [(Float, Float)]
parabolaPoints2 posX posY anguloInicial =
    [(x, parabola x posX posY anguloInicial) | x <- [posX, posX - 10 .. posX - 300]]
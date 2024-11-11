module Disparo where

velocidadInicial :: Float
velocidadInicial = 100

-- Función de la parábola que depende del ángulo inicial, puntos iniciales y velocidad
parabola :: Float -> Float -> Float -> Float -> Float
parabola x anguloInicial puntoInicialX puntoInicialY = puntoInicialY + tan anguloInicial * (x - puntoInicialX) - (gravedad / (2 * (velocidadInicial * cos anguloInicial) ^ 2)) * (x - puntoInicialX) ^ 2
  where
    gravedad = 9.8

-- Genera puntos de la parábola a partir de la posición inicial
puntosParabola :: Float -> Float -> Float -> Float -> [(Float, Float)]
puntosParabola posX posY anguloInicial lado = [(x, (parabola x anguloInicial posX posY)) | x <- [posX, posX + (10*lado) .. posX + (300*lado)]]
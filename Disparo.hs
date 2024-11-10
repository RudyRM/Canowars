module Disparo where
import Graphics.Gloss
import Types

moveTank :: Jugador -> Float -> Jugador
moveTank jugador dx
    | combustible jugador > 0 = jugador 
        { posX = posX jugador + dx
        , combustible = combustible jugador - 1 
        }
    | otherwise = jugador

velocidadInicial :: Float
velocidadInicial = 100  -- Puedes ajustar la velocidad según lo necesites

-- Función de la parábola que depende del ángulo inicial y la velocidad
parabola :: Float -> Float -> Float -> Float -> Float
parabola x anguloInicial puntoInicialX puntoInicialY = puntoInicialY + tan anguloInicial * (x - puntoInicialX) - (gravedad / (2 * (velocidadInicial * cos anguloInicial) ^ 2)) * (x - puntoInicialX) ^ 2
  where
    gravedad = 9.8

-- Genera puntos de la parábola a partir de la posición inicial
parabolaPoints :: Float -> Float -> Float -> Float -> [(Float, Float)]
parabolaPoints posX posY anguloInicial lado = [(x, (parabola x anguloInicial posX posY)) | x <- [posX, posX + (10*lado) .. posX + (300*lado)]]
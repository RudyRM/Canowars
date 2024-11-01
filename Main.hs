module Main where

import Graphics.Gloss.Interface.Pure.Game
import Menu
import NextScreen

data GameState = Menu | NextScreen

main :: IO ()
main = do
  play
    (InWindow "Pantalla" (800, 600) (100, 100))  -- Tamaño y posición de la ventana
    white                                          -- Color de fondo
    60                                             -- Velocidad de fotogramas
    Menu                                           -- Estado inicial
    draw                                           -- Función de dibujo
    handleInput                                    -- Manejo de eventos
    update                                         -- Función de actualización

draw :: GameState -> Picture
draw Menu      = drawMenu  -- Dibuja el menú
draw NextScreen = drawNextScreen  -- Dibuja la pantalla siguiente

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = NextScreen
handleInput (EventKey (Char 'q') Down _ _) NextScreen = Menu  -- Cambia de nuevo al menú con la tecla 'q'
handleInput (EventKey (SpecialKey KeySpace) Down _ _) NextScreen = NextScreen  -- Otras teclas en NextScreen
handleInput _ state = state

update :: Float -> GameState -> GameState
update _ state = state

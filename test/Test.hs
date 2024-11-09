import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Definir el estado del juego
data GameState = GameState
  { playerX :: Float  -- Posición del personaje en X
  , movingLeft :: Bool -- Indica si la tecla de izquierda está presionada
  , movingRight :: Bool -- Indica si la tecla de derecha está presionada
  }

-- Configurar el estado inicial del juego
initialState :: GameState
initialState = GameState 0 False False

-- Tamaño de la ventana
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

-- Configuración de la ventana
window :: Display
window = InWindow "Mi Juego en Haskell" (windowWidth, windowHeight) (10, 10)

-- Fondo de la ventana
background :: Color
background = white

-- Dibujo del estado del juego
render :: GameState -> Picture
render state = translate (playerX state) 0 $ color blue $ circleSolid 20

-- Función que maneja la entrada del usuario (teclas presionadas)
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = state { movingRight = True }
handleInput (EventKey (SpecialKey KeyRight) Up _ _) state = state { movingRight = False }
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = state { movingLeft = True }
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) state = state { movingLeft = False }
handleInput _ state = state  -- No cambiar el estado por otros eventos

-- Función para actualizar el estado del juego
update :: Float -> GameState -> GameState
update _ state =
  let
    -- Velocidad de movimiento más lenta (modificado de 10 a 5)
    speed = 5
    -- Movimiento solo en el eje X basado en las teclas presionadas
    dx = if movingRight state then speed else if movingLeft state then -speed else 0
    -- Calcular la nueva posición, limitando los bordes
    newX = playerX state + dx
    -- Limitar la posición para que no se salga de la ventana
    clampedX = max (-fromIntegral (windowWidth `div` 2) + 20) $ min (fromIntegral (windowWidth `div` 2) - 20) newX
  in state { playerX = clampedX }

-- Función principal para ejecutar el juego
main :: IO ()
main = play window background 60 initialState render handleInput update
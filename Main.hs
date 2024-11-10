import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Menu
import NextScreen
import Disparo
import System.IO.Unsafe (unsafePerformIO)
import InGame (gameDisplay)
import CanonSelect (drawCanonSelectionScreen)
import ScenarioSelect (drawScenarioSelect, drawScenarioSelectionScreen, drawScenarioSummary, randomSelectScenario)
import Types (CannonType(..), Turno(..), ScenarioType(..), Jugador(..), Proyectil(..))

data GameState = Menu 
                | CanonSelect Turno (Maybe Jugador)
                | ScenarioSelect Jugador Jugador
                | InGame Turno Jugador Jugador Picture (Maybe Proyectil) (Bool, Bool, Bool, Bool)  -- (upPressed, downPressed)

main :: IO ()
main = do
  play
    (InWindow "Pantalla" (1280, 720) (100, 100))
    white
    60
    Menu
    draw
    handleInput
    update

draw :: GameState -> Picture
draw Menu = drawMenu
draw (CanonSelect _ _) = drawCanonSelectionScreen
draw (ScenarioSelect _ _) = drawScenarioSelect
draw (InGame turno p1Cannon p2Cannon escenario proyectil movimiento) = gameDisplay turno p1Cannon p2Cannon escenario proyectil

handleInput :: Event -> GameState -> GameState
-- Transición al modo de selección de cañones desde el menú
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = CanonSelect Jugador1 Nothing
-- Volver al menú desde la selección de cañones o escenarios
handleInput (EventKey (Char 'q') Down _ _) (CanonSelect _ _) = Menu
handleInput (EventKey (Char 'q') Down _ _) (ScenarioSelect _ _) = Menu
handleInput (EventKey (Char 'q') Down _ _) (InGame _ _ _ _ _ _) = Menu
-- Selección de cañonesa
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Jugador1 _) = CanonSelect Jugador2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.9, proyectil = Nothing, posX = -350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_nazi.bmp")})
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Jugador1 _) = CanonSelect Jugador2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.9, proyectil = Nothing, posX = -350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_comunista.bmp")})
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Jugador1 _) = CanonSelect Jugador2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.9, proyectil = Nothing, posX = -350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_vaticano.bmp")})
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Jugador1 _) = CanonSelect Jugador2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.9, proyectil = Nothing, posX = -350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_eeuu.bmp")})
-- Cuando el jugador 2 selecciona su cañón, pasa a la selección de escenario
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Jugador2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = -0.9, proyectil = Nothing, posX = 350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_nazi.bmp")})
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Jugador2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = -0.9, proyectil = Nothing, posX = 350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_comunista.bmp")})
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Jugador2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = -0.9, proyectil = Nothing, posX = 350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_vaticano.bmp")})
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Jugador2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = -0.9, proyectil = Nothing, posX = 350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_eeuu.bmp")})
-- Selección de escenarios
handleInput (EventKey (Char '1') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame Jugador1 p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War1/Bright/War.bmp") Nothing (False, False, False, False)
handleInput (EventKey (Char '2') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame Jugador1 p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War2/Bright/War2.bmp") Nothing (False, False, False, False)
handleInput (EventKey (Char '3') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame Jugador1 p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War3/Bright/War3.bmp") Nothing (False, False, False, False)
handleInput (EventKey (Char '4') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame Jugador1 p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War4/Bright/War4.bmp") Nothing (False, False, False, False)

handleInput (EventKey (Char 'w') Down _ _) (InGame Jugador1 p1Cannon p2Cannon escenario proyectil (_, downPressed, leftPressed, rightPressed)) = 
  InGame Jugador1 p1Cannon p2Cannon escenario proyectil (True, downPressed, leftPressed, rightPressed)
handleInput (EventKey (Char 'w') Up _ _) (InGame Jugador1 p1Cannon p2Cannon escenario proyectil (_, downPressed, leftPressed, rightPressed)) = 
  InGame Jugador1 p1Cannon p2Cannon escenario proyectil (False, downPressed, leftPressed, rightPressed)
handleInput (EventKey (Char 's') Down _ _) (InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, _, leftPressed, rightPressed)) = 
  InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, True, leftPressed, rightPressed)
handleInput (EventKey (Char 's') Up _ _) (InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, _, leftPressed, rightPressed)) = 
  InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, False, leftPressed, rightPressed)

handleInput (EventKey (Char 'a') Down _ _) (InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, _, rightPressed)) =  
  InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, True, rightPressed)
handleInput (EventKey (Char 'a') Up _ _) (InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, _, rightPressed)) =  
  InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, False, rightPressed)
handleInput (EventKey (Char 'd') Down _ _) (InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, _)) =  
  InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, True)
handleInput (EventKey (Char 'd') Up _ _) (InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, _)) =  
  InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, False)

handleInput (EventKey (Char 'i') Down _ _) (InGame Jugador2 p1Cannon p2Cannon escenario proyectil (_, downPressed, leftPressed, rightPressed)) = 
  InGame Jugador2 p1Cannon p2Cannon escenario proyectil (True, downPressed, leftPressed, rightPressed)
handleInput (EventKey (Char 'i') Up _ _) (InGame Jugador2 p1Cannon p2Cannon escenario proyectil (_, downPressed, leftPressed, rightPressed)) = 
  InGame Jugador2 p1Cannon p2Cannon escenario proyectil (False, downPressed, leftPressed, rightPressed)
handleInput (EventKey (Char 'k') Down _ _) (InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, _, leftPressed, rightPressed)) = 
  InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, True, leftPressed, rightPressed)
handleInput (EventKey (Char 'k') Up _ _) (InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, _, leftPressed, rightPressed)) = 
  InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, False, leftPressed, rightPressed)

handleInput (EventKey (Char 'j') Down _ _) (InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, _, rightPressed)) =  
  InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, True, rightPressed)
handleInput (EventKey (Char 'j') Up _ _) (InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, _, rightPressed)) =  
  InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, False, rightPressed)
handleInput (EventKey (Char 'l') Down _ _) (InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, _)) =  
  InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, True)
handleInput (EventKey (Char 'l') Up _ _) (InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, _)) =  
  InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, False)

handleInput (EventKey (SpecialKey KeyEnter) Down _ _) (InGame Jugador1 p1Cannon p2Cannon escenario Nothing _) =
  InGame Jugador1 p1Cannon p2Cannon escenario 
    (Just (Proyectil { dañoProyectil = (daño p1Cannon), 
      posIniX = (posX p1Cannon),
      posXProyectil = (posX p1Cannon), 
      posYProyectil = (-250), 
      spriteProyectil = (unsafePerformIO $ loadBMP "assets/tanques/proyectil.bmp"),
      anguloProyectil = angulo p1Cannon  -- Guarda el ángulo inicial aquí
    })) (False, False, False, False)

handleInput (EventKey (SpecialKey KeyEnter) Down _ _) (InGame Jugador2 p1Cannon p2Cannon escenario Nothing _) =
  InGame Jugador2 p1Cannon p2Cannon escenario 
    (Just (Proyectil { dañoProyectil = (daño p2Cannon), 
      posIniX = (posX p2Cannon),
      posXProyectil = (posX p2Cannon), 
      posYProyectil = (-250), 
      spriteProyectil = (unsafePerformIO $ loadBMP "assets/tanques/proyectil.bmp"),
      anguloProyectil = angulo p2Cannon  -- Guarda el ángulo inicial aquí
    })) (False, False, False, False)

handleInput _ state = state

movimientoIzquierda Jugador1 posX = max (-600) (posX - 5)
movimientoIzquierda Jugador2 posX = max (80) (posX - 5)
movimientoDerecha Jugador1 posX = min (-80) (posX + 5)
movimientoDerecha Jugador2 posX = min (600) (posX + 5)

-- Actualización del estado del juego
update :: Float -> GameState -> GameState
update _ (InGame Jugador1 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, rightPressed)) =
  let 
    -- Actualizar el ángulo y posición de p1Cannon
    nuevoAngulo = if upPressed then angulo p1Cannon + 0.01
                  else if downPressed then angulo p1Cannon - 0.01
                  else angulo p1Cannon

    nuevaPosX = if leftPressed then movimientoIzquierda Jugador1 (posX p1Cannon)
                else if rightPressed then movimientoDerecha Jugador1 (posX p1Cannon)
                else posX p1Cannon

    nuevoCombustible = if upPressed || downPressed || leftPressed || rightPressed then combustible p1Cannon - 1
                       else combustible p1Cannon

    nuevoP1Cannon = p1Cannon { angulo = nuevoAngulo, combustible = nuevoCombustible, posX = nuevaPosX }

    -- Actualizar la posición del proyectil
    nuevoProyectil = case proyectil of
      Just p | posYProyectil p >= -250 -> Just p { 
        posXProyectil = (posXProyectil p + 2 + (abs (posXProyectil p)) * 0.01),
        posYProyectil = parabola (posXProyectil p) (anguloProyectil p) (posIniX p) (-250)
      }
      _ -> Nothing

    -- Verificar si el proyectil impacta el p2Cannon
    p2CannonImpactado = case nuevoProyectil of
      Just p | posXProyectil p >= (posX p2Cannon - 50) && posXProyectil p <= (posX p2Cannon + 50) -> True
      _ -> False

    -- Reducir el campo de vida de p2Cannon si es impactado
    nuevoP2Cannon = if p2CannonImpactado
                    then p2Cannon { vida = max 0 (vida p2Cannon - 1) }
                    else p2Cannon

  in if nuevoCombustible == 0 
    then InGame Jugador2 (nuevoP1Cannon { combustible = 150 }) nuevoP2Cannon escenario nuevoProyectil (False, False, False, False)
    else InGame Jugador1 nuevoP1Cannon nuevoP2Cannon escenario nuevoProyectil (upPressed, downPressed, leftPressed, rightPressed)


update _ (InGame Jugador2 p1Cannon p2Cannon escenario proyectil (upPressed, downPressed, leftPressed, rightPressed)) =
  let 
    -- Actualizar el ángulo y posición de p2Cannon
    nuevoAngulo = if upPressed then angulo p2Cannon + 0.01
                  else if downPressed then angulo p2Cannon - 0.01
                  else angulo p2Cannon

    nuevaPosX = if leftPressed then movimientoIzquierda Jugador2 (posX p2Cannon)
                else if rightPressed then movimientoDerecha Jugador2 (posX p2Cannon)
                else posX p2Cannon

    nuevoCombustible = if upPressed || downPressed || leftPressed || rightPressed then combustible p2Cannon - 1
                       else combustible p2Cannon

    nuevoP2Cannon = p2Cannon { angulo = nuevoAngulo, combustible = nuevoCombustible, posX = nuevaPosX }

    -- Actualizar la posición del proyectil
    nuevoProyectil = case proyectil of
      Just p | posYProyectil p >= -250 -> Just p { 
        posXProyectil = (posXProyectil p - 2 - (abs (posXProyectil p)) * 0.01),
        posYProyectil = parabola (posXProyectil p) (anguloProyectil p) (posIniX p) (-250)
      }
      _ -> Nothing

    -- Verificar si el proyectil impacta el p1Cannon
    p1CannonImpactado = case nuevoProyectil of
      Just p | posXProyectil p >= (posX p1Cannon - 50) && posXProyectil p <= (posX p1Cannon + 50) -> True
      _ -> False

    -- Reducir el campo de vida de p1Cannon si es impactado
    nuevoP1Cannon = if p1CannonImpactado
                    then p1Cannon { vida = max 0 (vida p1Cannon - 1) }
                    else p1Cannon

  in if nuevoCombustible == 0 
    then InGame Jugador1 nuevoP1Cannon (nuevoP2Cannon { combustible = 150 }) escenario nuevoProyectil (False, False, False, False)
    else InGame Jugador2 nuevoP1Cannon nuevoP2Cannon escenario nuevoProyectil (upPressed, downPressed, leftPressed, rightPressed)
-- Otros estados no cambian en el tiempo
update _ state = state
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)
import InGame (gameDisplay)
import Menu (dibujarMenu)
import SeleccionCanon (dibujarSeleccionDeCanon)
import SeleccionEscenario (dibujarSeleccionDeEscenario)
import Imagenes (mapa1, mapa2, mapa3, mapa4, tanque1, tanque2, tanque3, tanque4, imgProyectil, imgProyectilCritico)
import Tipos (Turno(..), Jugador(..), Proyectil(..))
import Disparo
import System.Random (randomR, mkStdGen, StdGen)
data GameState = Menu 
                | SeleccionCanon Turno (Maybe Jugador)
                | SeleccionEscenario Jugador Jugador
                | InGame Turno Jugador Jugador Picture (Maybe Proyectil) (Bool, Bool, Bool, Bool)

main :: IO ()
main = do
  play
    (InWindow "Canowars" (1280, 720) (100, 100))
    white
    60
    Menu
    dibujar
    manejarInput
    actualizar

dibujar :: GameState -> Picture
dibujar Menu = dibujarMenu
dibujar (SeleccionCanon _ _) = dibujarSeleccionDeCanon
dibujar (SeleccionEscenario _ _) = dibujarSeleccionDeEscenario
dibujar (InGame turno canonP1 canonP2 escenario proyectilJugador movimiento) = gameDisplay turno canonP1 canonP2 escenario proyectilJugador

-- Devuelve el nuevo ángulo ajustado y la semilla actualizada
anguloRandom :: Float -> Float -> StdGen -> (Float, StdGen)
anguloRandom angulo beta semilla = (angulo + randomOffset, nuevaSemilla)
  where
    -- Generamos un valor aleatorio dentro del rango usando la semilla actual
    (randomOffset, nuevaSemilla) = randomR ((-beta), beta) semilla

danoRandom :: StdGen -> (Int, StdGen)
danoRandom semilla =
  if probabilidad < 0.2
    then (danoCritico, nuevaSemilla)
    else (danoNormal, nuevaSemilla)
  where
    (probabilidad, semillaProbabilidad) = randomR (0.0 :: Float, 1.0) semilla
    (danoCritico, semillaCritico) = randomR (7, 9) semilla
    (danoNormal, nuevaSemilla) = randomR (1, 3) semilla

iniciarJugador1 :: Picture -> GameState
iniciarJugador1 sprite = 
  SeleccionCanon Jugador2 (Just Jugador { vida = 100, combustibleJugador = 200, anguloJugador = 1, semillaDesvioAngulo = mkStdGen 1, semillaCritico = mkStdGen 1, posXJugador = -350, spriteJugador = sprite})

iniciarJugador2 :: Picture -> GameState
iniciarJugador2 canonP1 sprite = 
  SeleccionEscenario canonP1 (Jugador { vida = 100, combustibleJugador = 200, anguloJugador = -1, semillaDesvioAngulo = mkStdGen 2, semillaCritico = mkStdGen 2, posXJugador = 350, spriteJugador = tanque1})

manejarInput :: Event -> GameState -> GameState
-- Transición al modo de selección de cañones desde el menú
manejarInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = SeleccionCanon Jugador1 Nothing

-- Volver al menú desde las pantallas
manejarInput (EventKey (Char 'q') Down _ _) (SeleccionCanon _ _) = Menu
manejarInput (EventKey (Char 'q') Down _ _) (SeleccionEscenario _ _) = Menu
manejarInput (EventKey (Char 'q') Down _ _) (InGame _ _ _ _ _ _) = Menu

-- Selección de cañon del jugador 1
manejarInput (EventKey (Char '1') Down _ _) (SeleccionCanon Jugador1 _) = 
  SeleccionCanon Jugador2 (Just Jugador { vida = 100, combustibleJugador = 200, anguloJugador = 1, semillaDesvioAngulo = mkStdGen 1, semillaCritico = mkStdGen 1, posXJugador = -350, spriteJugador = tanque1})
manejarInput (EventKey (Char '2') Down _ _) (SeleccionCanon Jugador1 _) = 
  SeleccionCanon Jugador2 (Just Jugador { vida = 100, combustibleJugador = 200, anguloJugador = 1, semillaDesvioAngulo = mkStdGen 1, semillaCritico = mkStdGen 1, posXJugador = -350, spriteJugador = tanque2})
manejarInput (EventKey (Char '3') Down _ _) (SeleccionCanon Jugador1 _) = 
  SeleccionCanon Jugador2 (Just Jugador { vida = 100, combustibleJugador = 200, anguloJugador = 1, semillaDesvioAngulo = mkStdGen 1, semillaCritico = mkStdGen 1, posXJugador = -350, spriteJugador = tanque3})
manejarInput (EventKey (Char '4') Down _ _) (SeleccionCanon Jugador1 _) = 
  SeleccionCanon Jugador2 (Just Jugador { vida = 100, combustibleJugador = 200, anguloJugador = 1, semillaDesvioAngulo = mkStdGen 1, semillaCritico = mkStdGen 1, posXJugador = -350, spriteJugador = tanque4})

-- Selección de cañon del jugador 2
manejarInput (EventKey (Char '1') Down _ _) (SeleccionCanon Jugador2 (Just canonP1)) = 
  SeleccionEscenario canonP1 (Jugador { vida = 100, combustibleJugador = 200, anguloJugador = -1, semillaDesvioAngulo = mkStdGen 2, semillaCritico = mkStdGen 2, posXJugador = 350, spriteJugador = tanque1})
manejarInput (EventKey (Char '2') Down _ _) (SeleccionCanon Jugador2 (Just canonP1)) = 
  SeleccionEscenario canonP1 (Jugador { vida = 100, combustibleJugador = 200, anguloJugador = -1, semillaDesvioAngulo = mkStdGen 2, semillaCritico = mkStdGen 2, posXJugador = 350, spriteJugador = tanque2})
manejarInput (EventKey (Char '3') Down _ _) (SeleccionCanon Jugador2 (Just canonP1)) = 
  SeleccionEscenario canonP1 (Jugador { vida = 100, combustibleJugador = 200, anguloJugador = -1, semillaDesvioAngulo = mkStdGen 2, semillaCritico = mkStdGen 2, posXJugador = 350, spriteJugador = tanque3})
manejarInput (EventKey (Char '4') Down _ _) (SeleccionCanon Jugador2 (Just canonP1)) = 
  SeleccionEscenario canonP1 (Jugador { vida = 100, combustibleJugador = 200, anguloJugador = -1, semillaDesvioAngulo = mkStdGen 2, semillaCritico = mkStdGen 2, posXJugador = 350, spriteJugador = tanque4})

-- Selección de escenarios
manejarInput (EventKey (Char '1') Down _ _) (SeleccionEscenario canonP1 canonP2) = 
  InGame Jugador1 canonP1 canonP2 mapa1 Nothing (False, False, False, False)
manejarInput (EventKey (Char '2') Down _ _) (SeleccionEscenario canonP1 canonP2) = 
  InGame Jugador1 canonP1 canonP2 mapa2 Nothing (False, False, False, False)
manejarInput (EventKey (Char '3') Down _ _) (SeleccionEscenario canonP1 canonP2) = 
  InGame Jugador1 canonP1 canonP2 mapa3 Nothing (False, False, False, False)
manejarInput (EventKey (Char '4') Down _ _) (SeleccionEscenario canonP1 canonP2) = 
  InGame Jugador1 canonP1 canonP2 mapa4 Nothing (False, False, False, False)

-- Manejar movimiento del jugador 1
manejarInput (EventKey (Char 'w') Down _ _) (InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (_, downPressed, leftPressed, rightPressed)) = 
  InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (True, downPressed, leftPressed, rightPressed)
manejarInput (EventKey (Char 'w') Up _ _) (InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (_, downPressed, leftPressed, rightPressed)) = 
  InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (False, downPressed, leftPressed, rightPressed)
manejarInput (EventKey (Char 's') Down _ _) (InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, _, leftPressed, rightPressed)) = 
  InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, True, leftPressed, rightPressed)
manejarInput (EventKey (Char 's') Up _ _) (InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, _, leftPressed, rightPressed)) = 
  InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, False, leftPressed, rightPressed)
manejarInput (EventKey (Char 'a') Down _ _) (InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, _, rightPressed)) =  
  InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, True, rightPressed)
manejarInput (EventKey (Char 'a') Up _ _) (InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, _, rightPressed)) =  
  InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, False, rightPressed)
manejarInput (EventKey (Char 'd') Down _ _) (InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, leftPressed, _)) =  
  InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, leftPressed, True)
manejarInput (EventKey (Char 'd') Up _ _) (InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, leftPressed, _)) =  
  InGame Jugador1 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, leftPressed, False)

-- Manejar movimiento del jugador 2
manejarInput (EventKey (Char 'i') Down _ _) (InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (_, downPressed, leftPressed, rightPressed)) = 
  InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (True, downPressed, leftPressed, rightPressed)
manejarInput (EventKey (Char 'i') Up _ _) (InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (_, downPressed, leftPressed, rightPressed)) = 
  InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (False, downPressed, leftPressed, rightPressed)
manejarInput (EventKey (Char 'k') Down _ _) (InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, _, leftPressed, rightPressed)) = 
  InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, True, leftPressed, rightPressed)
manejarInput (EventKey (Char 'k') Up _ _) (InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, _, leftPressed, rightPressed)) = 
  InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, False, leftPressed, rightPressed)
manejarInput (EventKey (Char 'j') Down _ _) (InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, _, rightPressed)) =  
  InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, True, rightPressed)
manejarInput (EventKey (Char 'j') Up _ _) (InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, _, rightPressed)) =  
  InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, False, rightPressed)
manejarInput (EventKey (Char 'l') Down _ _) (InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, leftPressed, _)) =  
  InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, leftPressed, True)
manejarInput (EventKey (Char 'l') Up _ _) (InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, leftPressed, _)) =  
  InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, leftPressed, False)

manejarInput (EventKey (SpecialKey KeyEnter) Down _ _) (InGame turno canonP1 canonP2 escenario Nothing _) =
  let 
    canonGatillador = if turno == Jugador1 then canonP1 else canonP2
    (angulo, semillaNuevaAngulo) = anguloRandom (anguloJugador canonGatillador) (0.05) (semillaDesvioAngulo canonGatillador)
    (dañoProyectil, semillaNuevaCritico) = danoRandom (semillaCritico canonGatillador)
    nuevoCannonP1 = if turno == Jugador1 then canonP1 { semillaDesvioAngulo = semillaNuevaAngulo, semillaCritico = semillaNuevaCritico} else canonP1
    nuevoCannonP2 = if turno == Jugador2 then canonP2 { semillaDesvioAngulo = semillaNuevaAngulo, semillaCritico = semillaNuevaCritico} else canonP2
    proyectil = 
      (Just (Proyectil { 
        dañoProyectil = dañoProyectil, 
        posIniXProyectil = (posXJugador canonGatillador),
        anguloIniProyectil = angulo,
        posXProyectil = (posXJugador canonGatillador), 
        posYProyectil = (-250), 
        spriteProyectil = if dañoProyectil < 4 then imgProyectil else imgProyectilCritico
      }))
  in
    InGame turno nuevoCannonP1 nuevoCannonP2 escenario proyectil (False, False, False, False)

manejarInput _ state = state

movimientoIzquierda :: Turno -> Float -> Float
movimientoIzquierda Jugador1 posXJugador = max (-600) (posXJugador - 5)
movimientoIzquierda Jugador2 posXJugador = max (80) (posXJugador - 5)

movimientoDerecha :: Turno -> Float -> Float
movimientoDerecha Jugador1 posXJugador = min (-80) (posXJugador + 5)
movimientoDerecha Jugador2 posXJugador = min (600) (posXJugador + 5)

-- Actualización del estado del juego
actualizar :: Float -> GameState -> GameState
actualizar _ (InGame turno canonP1 canonP2 escenario proyectil (upPressed, downPressed, leftPressed, rightPressed)) =
  let 
    siguienteTurno = if turno == Jugador1 then Jugador2 else Jugador1
    canonJugando = if turno == Jugador1 then canonP1 else canonP2
    canonEstatico = if turno == Jugador1 then canonP2 else canonP1
    lado = if turno == Jugador1 then 1 else (-1)

    -- Actualizar el ángulo y posición de canonP1
    nuevoAngulo = if upPressed then anguloJugador canonJugando + 0.01
                  else if downPressed then anguloJugador canonJugando - 0.01
                  else anguloJugador canonJugando

    nuevaPosX = if leftPressed then movimientoIzquierda turno (posXJugador canonJugando)
                else if rightPressed then movimientoDerecha turno (posXJugador canonJugando)
                else posXJugador canonJugando

    nuevoCombustible = if upPressed || downPressed || leftPressed || rightPressed then combustibleJugador canonJugando - 1
                       else combustibleJugador canonJugando

    nuevoCanonJugando = canonJugando { anguloJugador = nuevoAngulo, combustibleJugador = nuevoCombustible, posXJugador = nuevaPosX }

    -- Actualizar la posición del proyectil
    nuevoProyectil = case proyectil of
      Just p | posYProyectil p >= -250 -> Just p { 
        posXProyectil = (posXProyectil p + (2*lado) + (abs (posXProyectil p)) * 0.01 * lado),
        posYProyectil = parabola (posXProyectil p) (anguloIniProyectil p) (posIniXProyectil p) (-250)
      }
      _ -> Nothing

    -- Verificar si el proyectil impacta el canonEstatico
    canonImpactado = case nuevoProyectil of
      Just p | posXProyectil p >= (posXJugador canonEstatico - 50) && posXProyectil p <= (posXJugador canonEstatico + 50) && posYProyectil p <= (-240) -> True
      _ -> False

    -- Reducir el campo de vida de canonEstatico si es impactado
    nuevoCanonImpactado = if canonImpactado
                          then canonEstatico { vida = max 0 (vida canonEstatico - (dañoProyectil proyectil)) }
                          else canonEstatico

  in if nuevoCombustible == 0 
    then InGame siguienteTurno (nuevoP1Cannon { combustibleJugador = 200 }) nuevoP2Cannon escenario nuevoProyectil (False, False, False, False)
    else InGame turno nuevoP1Cannon nuevoP2Cannon escenario nuevoProyectil (upPressed, downPressed, leftPressed, rightPressed)


actualizar _ (InGame Jugador2 canonP1 canonP2 escenario proyectilJugador (upPressed, downPressed, leftPressed, rightPressed)) =
  let 
    -- Actualizar el ángulo y posición de canonP2
    nuevoAngulo = if upPressed then anguloJugador canonP2 + 0.01
                  else if downPressed then anguloJugador canonP2 - 0.01
                  else anguloJugador canonP2

    nuevaPosX = if leftPressed then movimientoIzquierda Jugador2 (posXJugador canonP2)
                else if rightPressed then movimientoDerecha Jugador2 (posXJugador canonP2)
                else posXJugador canonP2

    nuevoCombustible = if upPressed || downPressed || leftPressed || rightPressed then combustibleJugador canonP2 - 1
                       else combustibleJugador canonP2

    nuevoP2Cannon = canonP2 { anguloJugador = nuevoAngulo, combustibleJugador = nuevoCombustible, posXJugador = nuevaPosX }

    -- Actualizar la posición del proyectilJugador
    nuevoProyectil = case proyectilJugador of
      Just p | posYProyectil p >= -250 -> Just p { 
        posXProyectil = (posXProyectil p - 2 - (abs (posXProyectil p)) * 0.01),
        posYProyectil = parabola (posXProyectil p) (anguloIniProyectil p) (posIniXProyectil p) (-250)
      }
      _ -> Nothing

    -- Verificar si el proyectilJugador impacta el canonP1
    p1CannonImpactado = case nuevoProyectil of
      Just p | posXProyectil p >= (posXJugador canonP1 - 50) && posXProyectil p <= (posXJugador canonP1 + 50) && posYProyectil p <= (-230) -> True
      _ -> False

    -- Reducir el campo de vida de canonP1 si es impactado
    nuevoP1Cannon = if p1CannonImpactado
                    then canonP1 { vida = max 0 (vida canonP1 - 1) }
                    else canonP1

  in if nuevoCombustible == 0 
    then InGame Jugador1 nuevoP1Cannon (nuevoP2Cannon { combustibleJugador = 200 }) escenario nuevoProyectil (False, False, False, False)
    else InGame Jugador2 nuevoP1Cannon nuevoP2Cannon escenario nuevoProyectil (upPressed, downPressed, leftPressed, rightPressed)
-- Otros estados no cambian en el tiempo
actualizar _ state = state
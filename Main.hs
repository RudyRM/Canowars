import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)
import InGame (gameDisplay)
import Menu (dibujarMenu)
import SeleccionCanon (dibujarSeleccionDeCanon)
import SeleccionEscenario (dibujarSeleccionDeEscenario)
import PantallaFinal (dibujarPantallaFinal)
import Imagenes (mapa1, mapa2, mapa3, mapa4, tanque1, tanque2, tanque3, tanque4, imgProyectil, imgProyectilCritico, jugador1, jugador2)
import Tipos (Turno(..), Jugador(..), Proyectil(..))
import Disparo
import System.Random (randomR, mkStdGen, StdGen)
data GameState = Menu 
                | SeleccionCanon Turno (Maybe Jugador)
                | SeleccionEscenario Jugador Jugador
                | InGame Turno Jugador Jugador Picture (Maybe Proyectil) (Bool, Bool, Bool, Bool)
                | PantallaFinal Jugador

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
dibujar (PantallaFinal ganador) = dibujarPantallaFinal ganador

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

manejarInput :: Event -> GameState -> GameState
-- Transición al modo de selección de cañones desde el menú
manejarInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = SeleccionCanon Jugador1 Nothing

-- Volver al menú desde las pantallas
manejarInput (EventKey (Char 'q') Down _ _) (SeleccionCanon _ _) = Menu
manejarInput (EventKey (Char 'q') Down _ _) (SeleccionEscenario _ _) = Menu
manejarInput (EventKey (Char 'q') Down _ _) (InGame _ _ _ _ _ _) = Menu
manejarInput (EventKey (Char 'q') Down _ _) (PantallaFinal _) = Menu

-- Selección de cañon del jugador 1
manejarInput (EventKey (Char '1') Down _ _) (SeleccionCanon Jugador1 _) = 
  SeleccionCanon Jugador2 (Just Jugador { vida = 100, combustibleJugador = 200, anguloJugador = 1, semillaDesvioAngulo = mkStdGen 1, semillaCritico = mkStdGen 1, posXJugador = -350, spriteJugador = tanque1, textoJugador = jugador1})
manejarInput (EventKey (Char '2') Down _ _) (SeleccionCanon Jugador1 _) = 
  SeleccionCanon Jugador2 (Just Jugador { vida = 100, combustibleJugador = 200, anguloJugador = 1, semillaDesvioAngulo = mkStdGen 1, semillaCritico = mkStdGen 1, posXJugador = -350, spriteJugador = tanque2, textoJugador = jugador1})
manejarInput (EventKey (Char '3') Down _ _) (SeleccionCanon Jugador1 _) = 
  SeleccionCanon Jugador2 (Just Jugador { vida = 100, combustibleJugador = 200, anguloJugador = 1, semillaDesvioAngulo = mkStdGen 1, semillaCritico = mkStdGen 1, posXJugador = -350, spriteJugador = tanque3, textoJugador = jugador1})
manejarInput (EventKey (Char '4') Down _ _) (SeleccionCanon Jugador1 _) = 
  SeleccionCanon Jugador2 (Just Jugador { vida = 100, combustibleJugador = 200, anguloJugador = 1, semillaDesvioAngulo = mkStdGen 1, semillaCritico = mkStdGen 1, posXJugador = -350, spriteJugador = tanque4, textoJugador = jugador1})

-- Selección de cañon del jugador 2
manejarInput (EventKey (Char '1') Down _ _) (SeleccionCanon Jugador2 (Just canonP1)) = 
  SeleccionEscenario canonP1 (Jugador { vida = 100, combustibleJugador = 200, anguloJugador = -1, semillaDesvioAngulo = mkStdGen 2, semillaCritico = mkStdGen 2, posXJugador = 350, spriteJugador = tanque1, textoJugador = jugador2})
manejarInput (EventKey (Char '2') Down _ _) (SeleccionCanon Jugador2 (Just canonP1)) = 
  SeleccionEscenario canonP1 (Jugador { vida = 100, combustibleJugador = 200, anguloJugador = -1, semillaDesvioAngulo = mkStdGen 2, semillaCritico = mkStdGen 2, posXJugador = 350, spriteJugador = tanque2, textoJugador = jugador2})
manejarInput (EventKey (Char '3') Down _ _) (SeleccionCanon Jugador2 (Just canonP1)) = 
  SeleccionEscenario canonP1 (Jugador { vida = 100, combustibleJugador = 200, anguloJugador = -1, semillaDesvioAngulo = mkStdGen 2, semillaCritico = mkStdGen 2, posXJugador = 350, spriteJugador = tanque3, textoJugador = jugador2})
manejarInput (EventKey (Char '4') Down _ _) (SeleccionCanon Jugador2 (Just canonP1)) = 
  SeleccionEscenario canonP1 (Jugador { vida = 100, combustibleJugador = 200, anguloJugador = -1, semillaDesvioAngulo = mkStdGen 2, semillaCritico = mkStdGen 2, posXJugador = 350, spriteJugador = tanque4, textoJugador = jugador2})

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

-- Terminar turno voluntariamente
manejarInput (EventKey (SpecialKey KeySpace) Down _ _) (InGame turno canonP1 canonP2 escenario proyectilJugador _) = 
  let     
    siguienteTurno = if turno == Jugador1 then Jugador2 else Jugador1
    nuevoCannonP1 = if turno == Jugador1 then canonP1 {combustibleJugador = 200} else canonP1
    nuevoCannonP2 = if turno == Jugador1 then canonP2 {combustibleJugador = 200} else canonP2
  in InGame siguienteTurno nuevoCannonP1 nuevoCannonP2 escenario proyectilJugador (False, False, False, False)

manejarInput (EventKey (SpecialKey KeyEnter) Down _ _) (InGame turno canonP1 canonP2 escenario Nothing _) =
  let 
    canonGatillador = if turno == Jugador1 then canonP1 else canonP2
    (angulo, semillaNuevaAngulo) = anguloRandom (anguloJugador canonGatillador) (0.05) (semillaDesvioAngulo canonGatillador)
    (dañoProyectil, semillaNuevaCritico) = danoRandom (semillaCritico canonGatillador)
    nuevoCombustible = (combustibleJugador canonGatillador) - 30
    nuevoCannonP1 = if turno == Jugador1 then canonP1 { semillaDesvioAngulo = semillaNuevaAngulo, semillaCritico = semillaNuevaCritico, combustibleJugador = if nuevoCombustible >= 0 then nuevoCombustible else (combustibleJugador canonGatillador)} else canonP1
    nuevoCannonP2 = if turno == Jugador2 then canonP2 { semillaDesvioAngulo = semillaNuevaAngulo, semillaCritico = semillaNuevaCritico, combustibleJugador = if nuevoCombustible >= 0 then nuevoCombustible else (combustibleJugador canonGatillador)} else canonP2
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
    if nuevoCombustible >= 0 then
      InGame turno nuevoCannonP1 nuevoCannonP2 escenario proyectil (False, False, False, False)
    else 
      InGame turno nuevoCannonP1 nuevoCannonP2 escenario Nothing (False, False, False, False)

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
    nuevoAngulo = if upPressed then anguloJugador canonJugando + 0.01*lado
                  else if downPressed then anguloJugador canonJugando - 0.01*lado
                  else anguloJugador canonJugando

    nuevaPosX = if leftPressed then movimientoIzquierda turno (posXJugador canonJugando)
                else if rightPressed then movimientoDerecha turno (posXJugador canonJugando)
                else posXJugador canonJugando

    nuevoCombustible = if upPressed || downPressed || leftPressed || rightPressed then combustibleJugador canonJugando - 1
                       else combustibleJugador canonJugando

    nuevoCanonJugando = canonJugando { anguloJugador = nuevoAngulo, combustibleJugador = if nuevoCombustible == 0 then 200 else nuevoCombustible, posXJugador = nuevaPosX }

    -- Actualizar la posición del proyectil
    nuevoProyectil = case proyectil of
      Just p | posYProyectil p >= -250 
            , not (posXProyectil p >= -55 && posXProyectil p <= 55 && posYProyectil p >= -250 && posYProyectil p <= 30) ->
                Just p { 
                  posXProyectil = (posXProyectil p + (2*lado) + (abs (posXProyectil p)) * 0.01 * lado),
                  posYProyectil = parabola (posXProyectil p) (anguloIniProyectil p) (posIniXProyectil p) (-250)
                }
      _ -> Nothing

    -- Verificar si el proyectil impacta el canonEstatico
    canonImpactado = case nuevoProyectil of
      Just p | posXProyectil p >= (posXJugador canonEstatico - 50) && posXProyectil p <= (posXJugador canonEstatico + 50) && posYProyectil p <= (-240) -> canonEstatico { vida = max 0 (vida canonEstatico - (dañoProyectil p)) }
      _ -> canonEstatico

    (nuevoCannonP1, nuevoCannonP2) = if turno == Jugador1 then (nuevoCanonJugando, canonImpactado) else (canonImpactado, nuevoCanonJugando)

  in 
    if vida canonImpactado == 0 
      then PantallaFinal canonJugando
    else if nuevoCombustible == 0 
      then InGame siguienteTurno nuevoCannonP1 nuevoCannonP2 escenario nuevoProyectil (False, False, False, False)
    else InGame turno nuevoCannonP1 nuevoCannonP2 escenario nuevoProyectil (upPressed, downPressed, leftPressed, rightPressed)

-- Otros estados no cambian en el tiempo
actualizar _ state = state
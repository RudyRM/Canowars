module Acciones (moveTank) where
import System.IO.Unsafe (unsafePerformIO)
import Graphics.Gloss (Picture)
import Types (Player(..), Jugador(..), Proyectil(..))

-- Mueve el tanque y reduce el combustible
moveTank :: Jugador -> Float -> Jugador
moveTank jugador dx
    | combustible jugador > 0 = jugador 
        { posX = posX jugador + dx
        , combustible = combustible jugador - 1 
        }
    | otherwise = jugador


disparar :: Picture -> Float -> Jugador -> Jugador
disparar sprite dt jugador
    | combustible jugador > 0 = 
        let nuevoProyectil = Just (crearProyectil jugador sprite)
            jugadorConProyectil = jugador {
                combustible = combustible jugador - 5,
                proyectil = nuevoProyectil
            }
        in moverProyectilJugador dt jugadorConProyectil -- Mueve el proyectil inmediatamente después de crearlo
    | otherwise = jugador

crearProyectil :: Jugador -> Picture -> Proyectil
crearProyectil jugador sprite = Proyectil {
    posXProyectil = posX jugador,
    posYProyectil = -250,  -- Posición inicial del proyectil (altura del cañón)
    velocidadX = 200 * cos (angulo jugador),
    velocidadY = 200 * sin (angulo jugador),
    dañoProyectil = daño jugador,
    spriteProyectil = sprite
}

moverProyectilJugador :: Float -> Jugador -> Jugador
moverProyectilJugador dt jugador =
    case proyectil jugador of
        Nothing -> jugador  -- Si no hay proyectil, no hace nada
        Just p ->
            let newX = posXProyectil p + velocidadX p * dt
                newY = posYProyectil p + velocidadY p * dt - 9.81 * dt^2 / 2  -- Añadir gravedad
                newVelY = velocidadY p - 9.81 * dt
            in if newY < -300  -- Si el proyectil cae por debajo de la pantalla
               then jugador { proyectil = Nothing }
               else jugador { proyectil = Just p { posXProyectil = newX, posYProyectil = newY, velocidadY = newVelY } }

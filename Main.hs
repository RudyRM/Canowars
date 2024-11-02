module Main where

import Graphics.Gloss.Interface.Pure.Game
import Menu
import NextScreen
import CanonSelect (drawCanonSelect, drawCanonSelectionScreen, drawSelectionSummary)
import Types (CannonType(..), Player(..)) -- Importa CannonType y Player

data GameState = Menu 
               | CanonSelect Player (Maybe CannonType) (Maybe CannonType) 
               | NextScreen CannonType CannonType


main :: IO ()
main = do
  play
    (InWindow "Pantalla" (800, 600) (100, 100))
    white
    60
    Menu
    draw
    handleInput
    update

draw :: GameState -> Picture
draw Menu                          = drawMenu
draw (CanonSelect currentPlayer _ _) = drawCanonSelectionScreen currentPlayer
draw (NextScreen p1Cannon p2Cannon) = drawSelectionSummary p1Cannon p2Cannon

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = CanonSelect Player1 Nothing Nothing
handleInput (EventKey (Char 'q') Down _ _) (CanonSelect _ _ _) = Menu
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just USA) p2Cannon
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just Italiano) p2Cannon
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just Frances) p2Cannon
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just Aleman) p2Cannon
handleInput (EventKey (Char '5') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just Chileno) p2Cannon
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = NextScreen p1Cannon USA
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = NextScreen p1Cannon Italiano
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = NextScreen p1Cannon Frances
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = NextScreen p1Cannon Aleman
handleInput (EventKey (Char '5') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = NextScreen p1Cannon Chileno
handleInput _ state = state

update :: Float -> GameState -> GameState
update _ state = state

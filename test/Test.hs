{-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Sound.OpenAL
import Control.Concurrent (forkIO, threadDelay)
import System.IO (withBinaryFile, IOMode(..), hGetContents)

-- Initialize audio
playBackgroundMusic :: FilePath -> IO ()
playBackgroundMusic path = do
    -- Load audio file (e.g., .wav)
    withBinaryFile path ReadMode $ \handle -> do
        contents <- hGetContents handle
        let bufferData = ... -- Parse and decode the audio data here
        -- Create buffer and source
        buffer <- genObjectName
        bufferData buffer $= bufferData
        source <- genObjectName
        buffer source $= Just buffer
        -- Play in a loop
        loopingMode source $= Looping
        play [source]

-- Main game loop (using gloss)
main :: IO ()
main = do
    -- Start background music in a separate thread
    _ <- forkIO $ playBackgroundMusic "assets/music/menu.wav"
    -- Run gloss window
    play display bgColor fps initialState render handleEvent update
  where
    display = InWindow "Game with Background Music" (640, 480) (100, 100)
    bgColor = white
    fps = 60
    initialState = ()
    render _ = Blank
    handleEvent _ s = s
    update _ s = s

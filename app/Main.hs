module Main where

import GUI (playPong)


main :: IO ()
main = do


    putStrLn ("\n" ++ "Launching GUI...")

    -- let windowSize = (1200, 800) :: (Int, Int)  -- To be move into Launcher
    playPong

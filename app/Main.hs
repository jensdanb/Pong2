module Main where

import GUI (playPong)


main :: IO ()
main = do
    putStrLn ("\n" ++ "Launching GUI...")
    playPong

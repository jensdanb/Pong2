module Main where


import Launcher (printBuildInfo)
import GUI (playPong)


main :: IO ()
main = do

    printBuildInfo

    putStrLn ("\n" ++ "Launching GUI...")

    -- let windowSize = (1200, 800) :: (Int, Int)  -- To be move into Launcher
    playPong

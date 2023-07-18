module Launcher (printBuildInfo) where


import System.Directory (listDirectory)
import Data.Char (isDigit)


printBuildInfo :: IO ()
printBuildInfo = do
    putStrLn ( "\n" ++ "Build Succesful. ")
    contents <- listDirectory "src"
    putStrLn ("Source files:" ++ show contents)



-- ASSUMES iMin < iMax
-- External function must provide appropriate iMin and iMax for its use.
safeUserInt :: (Int, Int) -> IO Int
safeUserInt (iMin, iMax) = do
    userInput <- getLine
    returnOrRetry userInput
        where returnOrRetry input
                | all isDigit input && intValue `elem` [iMin..iMax] = return intValue
                | otherwise                                  = safeUserInt (iMin, iMax)
                where intValue = read input :: Int



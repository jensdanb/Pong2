module GUI (playPong, Ball) where


import Graphics.Gloss (display, animate, simulate, Display(InWindow))
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Physics (Ball(Ball), Player(Player), GameState, updateGame, courtYard, ballSize, playerSize, xPosPlayer)


windowSize = (1200, 800)

windowDisplay :: Display
windowDisplay = InWindow "Game Window" windowSize (300, 200)


drawingFunc :: GameState -> Picture
drawingFunc (ball, player1, player2) = Pictures [courtYard :: Picture, drawScore ball, drawBall ball, drawPlayer player1, drawPlayer player2]
    where drawBall (Ball (x,y) _ _ _) = translate x y $ Circle ballSize
          drawPlayer (Player (x1,y1) _) = Line $ [(x1, y1-playerSize), (x1, y1+playerSize)]
          drawScore (Ball _ _ _ (pC, (s1,s2))) = scoreBoard $ Text ("Player1: " ++ show s1 ++ "   Player 2: " ++ show s2)
          scoreBoard = (translate (-250) 100) . (scale 0.3 0.3)


inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeyUp)    Down _ _) (ball, (Player pPos (x1',y1')), p2) = (ball, Player pPos (x1',y1'+280), p2)
inputHandler (EventKey (SpecialKey KeyDown)  Down _ _) (ball, (Player pPos (x1',y1')), p2) = (ball, Player pPos (x1',y1'-280), p2)
inputHandler _ w = w


playPong :: IO ()
playPong = do

    -- animate windowDisplay aquamarine animateCircle
    let startBall = Ball (0,200) (-360, 230) (0,-160) (0,(0,0))
    let player1 = Player (-xPosPlayer, 0) (0,0)
    let player2 = Player (xPosPlayer, 0) (0,270)
    let gameState = (startBall, player1, player2) :: GameState
    play windowDisplay orange 100 (startBall, player1, player2) drawingFunc inputHandler updateGame

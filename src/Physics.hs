module Physics (
     updateGame,                                  -- Functions
     courtYard, ballSize, playerSize, xPosPlayer, -- Constants
     Ball(Ball), Player(Player), GameState        -- Types
     ) where

import Graphics.Gloss.Data.Picture (Picture, Path, Vector, rectanglePath, rectangleWire)


--- Constants ---

courtWidth = 1100 :: Float
courtHeight = 700 :: Float

courtPath = rectanglePath courtWidth courtHeight :: Path
courtYard = rectangleWire courtWidth courtHeight :: Picture

collideRight  =  courtWidth / 2 - ballSize  :: Float  -- 550 - 12 = 538
collideLeft   = -collideRight               :: Float
collideTop    =  courtHeight / 2 - ballSize :: Float  -- 350 - 12 = 358
collideBottom = -collideTop                 :: Float


ballSize = 12                     :: Float
playerSize = 60                   :: Float
xPosPlayer = courtWidth / 2 - 90  :: Float


--- Types ---

data Ball = Ball
    { pos :: Vector
    , vel :: Vector
    , acc :: Vector
    , bMD :: BallMetaData
    }

type BallMetaData = (Int, (Int, Int))
-- (pC, (Player1 score, Player2 score))
-- pC explained: Previous Collision. What object did it hit last? Cannot hit same player twice in row.
-- 0: Wall. Can hit anything.
-- 1: Player1. Can't hit Player1
-- 2: Player2. Can't hit Player2


data Player = Player
    { pPos :: Vector
    , pVel :: Vector
    }


type GameState = (Ball, Player, Player)


--- Exposed Functions ---

updateGame :: Float -> GameState -> GameState
updateGame dt = collisionCheck . (moveAll dt)  --  <-GameState


--- Motion Functions ---

dtMotion :: Float -> Vector -> Vector -> Vector
dtMotion dt (x, y) (x', y') = (x + x' * dt, y + y' * dt)
-- Example: dtMotion position velocity     -> newPosition
--          dtMotion velocity acceleration -> newVelocity


moveAll :: Float -> GameState -> GameState
moveAll dt (ball, player1, player2) = (ball', player1', player2')
    where ball'   = moveBall ball
          player1' = movePlayer player1
          player2' = movePlayer player2

          moveBall (Ball pos vel acc bMD) = Ball (dtMotion dt pos vel) (dtMotion dt vel acc) acc bMD
          movePlayer (Player pPos pVel) = Player (dtMotion dt pPos pVel) pVel


xInvert :: Vector -> Vector
xInvert (x, y) = (-x, y)

yInvert :: Vector -> Vector
yInvert (x, y) = (x, -y)


--- Collision Functions ---

collisionCheck :: GameState -> GameState
collisionCheck gameState@(ball, player1, player2) = (ball', player1', player2')
    where
        player1' = playerWallCollision player1
        player2' = playerWallCollision player2

        ball'
            | ballsInDanger ball = ballCollision gameState
            | otherwise          = ball

        ballsInDanger :: Ball -> Bool
        ballsInDanger (Ball (x, y) _ _ _)
            | abs x > xPosPlayer - ballSize = True
            | abs y > collideTop            = True
            | otherwise                     = False


ballCollision :: GameState -> Ball
ballCollision (Ball pos@(x, y) vel@(x', y') acc (pC, scores@(s1, s2)), Player pos1@(x1, y1) vel1, Player pos2@(x2, y2) vel2)
    | ballHitsPlayer1    = Ball pos (xInvert vel) acc (1,  (s1+1, s2) )
    | ballHitsPlayer2    = Ball pos (xInvert vel) acc (2,  (s1, s2+1) )
    | sideWallCollision  = Ball pos (xInvert vel) acc (0,  scores )
    | topDownCollision   = Ball pos (yInvert vel) acc (0,  scores )
    | otherwise          = Ball pos vel           acc (pC, scores )
    where
        ballHitsPlayer1 = pC /= 1 && abs (x - x1) < ballSize && abs (y - y1) < playerSize + ballSize
        ballHitsPlayer2 = pC /= 2 && abs (x - x2) < ballSize && abs (y - y2) < playerSize + ballSize
        sideWallCollision = x >= collideRight && x' > 0 || x <= collideLeft   && x' < 0
        topDownCollision  = y >= collideTop   && y' > 0 || y <= collideBottom && y' < 0


playerWallCollision :: Player -> Player
playerWallCollision player@(Player pos@(x, y) vel@(x', y'))
    | triesToEscape = Player pos (yInvert vel)
    | otherwise     = player
    where
        triesToEscape = y' > 0 &&  y + playerSize > collideTop || y' < 0 &&  y - playerSize < collideBottom


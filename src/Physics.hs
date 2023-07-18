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
-- (bMD, (Player1 score, Player2 score))
-- bMD explained: Previous Collision. What object did it hit last? Cannot hit same player twice in row.
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


--- Collision Functions ---

collisionCheck :: GameState -> GameState
collisionCheck (ball, player1, player2) = (ball', player1', player2')
    where
        ball' = ballCollision (ballsInDanger ball) (ball, player1, player2)
        ballsInDanger :: Ball -> Bool
        ballsInDanger (Ball (x, y) _ _ _)
            | abs x > xPosPlayer - ballSize = True
            | abs y > collideTop            = True
            | otherwise                     = False

        player1' = playerWallCollision (playerInDanger player1) player1
        player2' = playerWallCollision (playerInDanger player2) player2
        playerInDanger :: Player -> Bool
        playerInDanger (Player (x, y) (x', y'))
            | y' > 0 &&  y + playerSize > collideTop    = True
            | y' < 0 &&  y - playerSize < collideBottom = True
            | otherwise                                 = False


ballCollision :: Bool -> GameState -> Ball
ballCollision False (ball, player1, player2) = ball
ballCollision _ (Ball (x, y) (x', y') acc (pC, (s1, s2)), Player (x1, y1) (x1', y1'), Player (x2, y2) (x2', y2'))
    | ballHitsPlayer1    = Ball (x, y) (-x', y') acc (1, (s1+1, s2))
    | ballHitsPlayer2    = Ball (x, y) (-x', y') acc (2, (s1, s2+1))
    | sideWallCollision  = Ball (x, y) (-x', y') acc (0, (s1, s2))
    | upDownCollision    = Ball (x, y) (x', -y') acc (0, (s1, s2))
    | otherwise          = Ball (x, y) (x', y') acc (pC, (s1, s2))
    where
        ballHitsPlayer1 = pC /= 1 && abs (x - x1) < ballSize && abs (y - y1) < playerSize + ballSize
        ballHitsPlayer2 = pC /= 2 && abs (x - x2) < ballSize && abs (y - y2) < playerSize + ballSize
        sideWallCollision = ( x >= collideRight && x' > 0 ) || ( x <= collideLeft && x' < 0 )
        upDownCollision =   ( y >= collideTop && y' > 0   ) || ( y <= collideBottom && y' < 0 )


playerWallCollision :: Bool -> Player -> Player
playerWallCollision False player = player
playerWallCollision _ (Player pPos (x1', y1')) = Player pPos (x1', -y1')

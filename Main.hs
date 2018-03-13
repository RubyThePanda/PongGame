module Main(main, PongGame(..), initialState, render) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

width, height, offset :: Int
width = 300
height = 300
offset = 100

window::Display
window = InWindow "Pong" (width,height) (offset,offset)

background::Color
background = black

-- | Data describing the state of the pong game.
data PongGame = Game
    { ballLoc::(Float, Float) -- ^ Pong ball (x, y) location.
    , ballVel::(Float, Float) -- ^ Pong ball (x, y) velocity.
    , player1::Float -- ^ Left player paddle height.
                     -- Zero is the middle of the screen.
    , player2::Float -- ^ Right player paddle height.
    , player1Vel::Float
    , player2Vel::Float
    , isPaused::Bool
    } deriving Show

-- | The starting state for the game of Pong.
initialState::PongGame
initialState = Game
    { ballLoc = (-40, -10)
    , ballVel = (50, -100)
    , player1 = 40
    , player2 = -80
    , player1Vel = 0
    , player2Vel = 0
    , isPaused = False
    }

-- | Update the ball position using its current velocity.
moveBall :: Float -- ^ The number of seconds since last update
    -> PongGame -- ^ The initial game state
    -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = if (isPaused game || (winner $ ballLoc game) > 0)
    then game
    else game{ ballLoc = (x', y') }
    where 
        -- Old locations and velocities.
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        -- New locations and velocities.
        x' = x + seconds * vx
        y' = y + seconds * vy

paddleBorderWidth, paddleBorderHeight :: Float
paddleBorderWidth = 26
paddleBorderHeight = 86

paddleOffset::Float
paddleOffset = 120

wallHeight :: Float
wallHeight = 10

-- | Convert a game state into a picture.
render :: PongGame -- ^ The game state to render.
    -> Picture -- ^ A picture of this game state.
render game = pictures
    [ ball, walls
    , mkPaddle green paddleOffset $ player1 game 
    , mkPaddle orange (-paddleOffset) $ player2 game
    ]
    where
        -- The pong ball
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
        ballColor = dark red

        -- The bottom and top walls
        wall::Float->Picture
        wall offset =
            translate 0 offset $
            color wallColor $
            rectangleSolid 270 wallHeight

        wallColor = greyN 0.5
        walls = pictures [wall 150, wall (-150)]

        -- Make a paddle of a given border and vertical offset
        mkPaddle::Color->Float->Float->Picture
        mkPaddle col x y = pictures
            [ translate x y $ color col $ rectangleSolid paddleBorderWidth paddleBorderHeight
            , translate x y $ color paddleColor $ rectangleSolid 20 80
            ]
        paddleColor = light $ light blue

-- | Number of frames to show per second.
fps :: Int
fps = 60

type Radius = Float
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision::Position->Radius->Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision = y - radius  <= - fromIntegral width / 2
        bottomCollision = y + radius >= fromIntegral width / 2

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
    where
        (vx, vy) = ballVel game
        radius = 10
        vy' = if wallCollision (ballLoc game) radius
            then -vy
            else vy

paddleCollision::Position
    -> Float -- y of left paddle
    -> Float -- y of right paddle
    ->Bool
paddleCollision (x, y) rpY lpY = leftCollision || rightCollision
    where
        leftCollision = (x <= -paddleOffset + paddleBorderWidth / 2) && yLeftCollision
        rightCollision = (x >= paddleOffset - paddleBorderWidth / 2) && yRightCollision
        yLeftCollision = (y <= lpY + paddleBorderHeight / 2) && (y >= lpY - paddleBorderHeight / 2)
        yRightCollision = (y <= rpY + paddleBorderHeight / 2) && (y >= rpY - paddleBorderHeight / 2)

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
    where
        (vx, vy) = ballVel game
        vx' = if paddleCollision (ballLoc game) (player1 game) (player2 game)
            then -vx
            else vx

winner::Position->Int
winner (x, y) = if x <= -(fromIntegral width / 2)
    then trace ("right player win!") 1
    else if x >= fromIntegral width / 2
    then trace ("left player win!") 2
    else 0

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> PongGame -> PongGame
update seconds  = paddleBounce . wallBounce . moveLeftPaddle . moveRightPaddle .  moveBall seconds

moveSpeed::Float
moveSpeed = 10

paddleTouchWallUp::Float->Bool
paddleTouchWallUp player = player >= ((fromIntegral height) - paddleBorderHeight) / 2 - wallHeight

paddleTouchWallBottom::Float->Bool
paddleTouchWallBottom player = player <= -((fromIntegral height) - paddleBorderHeight) / 2 + wallHeight 

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 's' keypress, reset the ball to the center.
-- TODO: bug?
handleKeys (EventKey(Char 's') _ _ _) game = game { ballLoc = (0, 0) }
handleKeys (EventKey(Char 'p') Down _ _) game = trace ("p is presssed set isPaused to " ++ show pausedState) game { isPaused = pausedState }
    where
        pausedState = if (isPaused game)
            then False
            else True

-- 'w' Move left paddle up
handleKeys (EventKey(Char 'w') Down _ _) game = game { player2Vel = moveSpeed }
handleKeys (EventKey(Char 'w') Up _ _) game = game { player2Vel = 0 }
 --   where
     --   player2' = if paddleTouchWallUp leftPlayer
      --      then leftPlayer
        --    else leftPlayer + moveSpeed

-- 'a' Move left paddle down
handleKeys (EventKey(Char 'a') Down _ _) game = game { player2Vel = -moveSpeed }
handleKeys (EventKey(Char 'a') Up _ _) game = game { player2Vel = 0 }
--    where
--        player2' = if paddleTouchWallBottom leftPlayer
--            then leftPlayer
 --           else leftPlayer - moveSpeed
  --      leftPlayer = player2 game

-- 'KeyUp' Move right paddle up
handleKeys (EventKey(SpecialKey KeyUp) _ _ _) game = game { player1 = player1' }
    where
        player1' = if paddleTouchWallUp rightPlayer
            then rightPlayer
            else rightPlayer + moveSpeed
        rightPlayer = player1 game
-- 'KeyDown' Move right paddle down
handleKeys (EventKey(SpecialKey KeyDown) _ _ _) game = game { player1 = player1' }
    where
        player1' = if paddleTouchWallBottom rightPlayer
            then rightPlayer
            else rightPlayer - moveSpeed
        rightPlayer = player1 game

-- Do nothing for all other events.
handleKeys _ game = game

moveLeftPaddle::PongGame->PongGame
moveLeftPaddle game = game { player2 = player2'}
    where
        player2' = if ((leftPlayerSpeed > 0) && not (paddleTouchWallUp leftPlayer))
            || ((leftPlayerSpeed < 0) && not (paddleTouchWallBottom leftPlayer))
            then leftPlayer + leftPlayerSpeed
            else leftPlayer
        leftPlayer = player2 game
        leftPlayerSpeed = player2Vel game

moveRightPaddle::PongGame->PongGame
moveRightPaddle game = game { player1 = player1'}
    where
        player1' = if ((rightPlayerSpeed > 0) && not (paddleTouchWallUp rightPlayer))
            || ((rightPlayerSpeed < 0) && not (paddleTouchWallBottom rightPlayer))
            then rightPlayer + rightPlayerSpeed
            else rightPlayer
        rightPlayer = player1 game
        rightPlayerSpeed = player1Vel game


main :: IO ()
main = play window background fps initialState render handleKeys update

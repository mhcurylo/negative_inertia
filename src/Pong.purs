module Pong(initPong, movePong, Vector, Pong, PongInput, PlayerInput) where
import Prelude ((&&), (>), (<), (+), (/), (*), (-), negate, pure, bind, Unit, discard, unit, ($), (>>=), otherwise)
import Data.Identity
import Data.Ord (clamp)
import Data.Tuple
import Data.Maybe
import Data.Array

type Vector = {
  x :: Number,
  y :: Number
}

origin :: Vector
origin = vec 0.0 0.0

type PlayerInput = {
  up :: Boolean,
  down :: Boolean
}

type PongInput = {
  player1 :: PlayerInput,
  player2 :: PlayerInput
}

type Pong = {
  canvasWidth :: Number,
  canvasHeight :: Number,
  ballSize :: Number,
  ballSpeed :: Number,
  paddleWidth :: Number,
  paddleHeight :: Number,
  paddleSpeed :: Number,
  paddle1 :: Vector,
  paddle2 :: Vector,
  score1 :: Int,
  score2 :: Int,
  ball :: Vector,
  ballVelocity :: Vector
}

paddleVelocity :: PlayerInput -> Number -> Number
paddleVelocity {up, down} paddleSpeed
  | up = -paddleSpeed
  | down = paddleSpeed
  | otherwise = 0.0

clampBallX :: Pong -> Pong
clampBallX game@{ball, ballSize, canvasWidth}
  | ball.x + ballSize < 0.0 = resetBall game { score1 = game.score1 + 1}
  | ball.x > canvasWidth = resetBall game { score2 = game.score2 + 1 }
  | otherwise = game

clampBallY :: Pong -> Pong
clampBallY game@{ball, ballSize, canvasHeight} =
  game {
    ball {
      y = clamp 0.0 (canvasHeight - ballSize) ball.y
    }
  }

ballVsWalls :: Pong -> Pong
ballVsWalls game@{ball, ballSize, ballVelocity, canvasHeight}
  | ball.y < 0.0 = game { ballVelocity {y = -ballVelocity.y }}
  | ball.y + ballSize > canvasHeight = game { ballVelocity { y = -ballVelocity.y }}
  | otherwise = game

moveBall :: Pong -> Pong
moveBall game = game {
    ball {
      x = game.ball.x + game.ballVelocity.x,
      y = game.ball.y + game.ballVelocity.y
    }
  }

paddle1VsBall :: Pong -> Pong
paddle1VsBall game@{ball, ballSize, ballVelocity, paddle1, paddleHeight, paddleWidth} = r
  where
    r = if intersects
        then game {
            ball {
              x = paddle1.x + paddleWidth
            },
            ballVelocity {
              x = -ballVelocity.x
            }
          }
        else game
    intersects =
      ball.x < paddle1.x + paddleWidth
        && ball.x > 0.0
        && ball.y + ballSize > paddle1.y
        && ball.y < paddle1.y + paddleHeight

paddle2VsBall :: Pong -> Pong
paddle2VsBall game@{ball, ballSize, ballVelocity, paddle2, paddleHeight, canvasWidth} = r
  where
    r = if intersects
        then game {
            ball {
              x = paddle2.x - ballSize
            },
            ballVelocity {
              x = -ballVelocity.x
            }
          }
        else game
    intersects =
      ball.x > paddle2.x - ballSize
        && ball.x + ballSize < canvasWidth
        && ball.y + ballSize > paddle2.y
        && ball.y < paddle2.y + paddleHeight

movePaddles :: Number -> Number -> Pong -> Pong
movePaddles vel1 vel2 game = game {
    paddle1 = movePaddle game.paddle1 vel1,
    paddle2 = movePaddle game.paddle2 vel2
  }
  where
    movePaddle :: Vector -> Number -> Vector
    movePaddle paddle@{y} velocity = paddle {
        y = clamp 0.0 (game.canvasHeight - game.paddleHeight) (y + velocity)
      }

vec :: Number -> Number -> Vector
vec x y = {x, y}

resetBall :: Pong -> Pong
resetBall game@{canvasWidth, canvasHeight, ballSize, ballSpeed} =
  game {
    ball = vec (canvasWidth * 0.5 - ballSize * 0.5) (canvasHeight * 0.5 - ballSize * 0.5),
    ballVelocity = vec (-ballSpeed) ballSpeed
  }

initPong :: Number -> Number -> Pong
initPong canvasWidth canvasHeight = resetBall o
  where
    s = canvasHeight / 150.0
    paddleWidth = 6.0 * s
    paddleHeight = 40.0 * s
    ballSize = 6.0 * s
    ballSpeed = 2.0 * s
    paddleY = (canvasHeight * 0.5 - paddleHeight * 0.5)
    o = {
      canvasWidth: canvasWidth,
      canvasHeight: canvasHeight,
      ballSize: ballSize,
      ballSpeed: ballSpeed,
      paddleWidth: 6.0 * s,
      paddleHeight: 40.0 * s,
      paddleSpeed: 3.0 * s,
      paddle1: vec 0.0 paddleY,
      paddle2: vec (canvasWidth - paddleWidth) paddleY,
      score1: 0,
      score2: 0,
      ball: origin,
      ballVelocity: origin
    }

movePong :: PongInput -> Pong -> Pong
movePong input game =
                  clampBallX
                $ clampBallY
                $ ballVsWalls
                $ paddle1VsBall
                $ paddle2VsBall
                $ movePaddles paddle1Velocity paddle2Velocity
                $ moveBall game
  where
    paddle1Velocity = paddleVelocity input.player1 game.paddleSpeed
    paddle2Velocity = paddleVelocity input.player2 game.paddleSpeed

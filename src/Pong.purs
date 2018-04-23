module Pong(initPong, movePong, Vector, Pong, PongInput, PlayerInput) where
import Prelude ((&&), (>), (<), (+), (/), (*), (-), negate, pure, bind, Unit, discard, unit, ($), (>>=), otherwise)
import Data.Identity
import Data.Ord (clamp)
import Data.Tuple
import Data.Array
import Partial.Unsafe (unsafePartial)

type Vector = {
  x :: Number,
  y :: Number
}

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
  paddles :: Array Vector,
  scores :: Array Int,
  ball :: Vector,
  ballVelocity :: Vector
}

paddleVelocity :: PlayerInput -> Number -> Number
paddleVelocity {up, down} paddleSpeed
  | up = -paddleSpeed
  | down = paddleSpeed
  | otherwise = 0.0

clampBall :: Pong -> Pong
clampBall game@{ball, ballSize, canvasHeight} =
  game {
    ball {
      y = clamp 0.0 (canvasHeight - ballSize) ball.y
    }
  }

ballVsWalls :: Pong -> Pong
ballVsWalls game@{ball, ballSize, ballVelocity, canvasHeight}
  | ball.y < 0.0 = game { ballVelocity { y = -ballVelocity.y }}
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
paddle1VsBall game@{ball, ballSize, ballVelocity, paddles, paddleHeight, paddleWidth} = r
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
    paddle1 = unsafePartial $ unsafeIndex paddles 0
    intersects =
      ball.x < paddle1.x + paddleWidth
        && ball.y + ballSize > paddle1.y
        && ball.y < paddle1.y + paddleHeight

paddle2VsBall :: Pong -> Pong
paddle2VsBall game@{ball, ballSize, ballVelocity, paddles, paddleHeight} = r
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
    paddle2 = unsafePartial $ unsafeIndex paddles 1
    intersects =
        ball.x > paddle2.x - ballSize
      && ball.y + ballSize > paddle2.y
      && ball.y < paddle2.y + paddleHeight

movePaddles :: Array Number -> Pong -> Pong
movePaddles velocities game = game {
    paddles = zipWith movePaddle game.paddles velocities
  }
  where
    movePaddle :: Vector -> Number -> Vector
    movePaddle paddle@{y} velocity = paddle {
        y = clamp 0.0 (game.canvasHeight - game.paddleHeight) (y + velocity)
      }

vec :: Number -> Number -> Vector
vec x y = {x, y}

initPong :: Number -> Number -> Pong
initPong canvasWidth canvasHeight = {
    canvasWidth: canvasWidth,
    canvasHeight: canvasHeight,
    ballSize: ballSize,
    ballSpeed: ballSpeed,
    paddleWidth: 6.0 * s,
    paddleHeight: 40.0 * s,
    paddleSpeed: 3.0 * s,
    paddles: [
      vec 0.0 paddleY,
      vec (canvasWidth - paddleWidth) paddleY
    ],
    scores: [
      0,
      0
    ],
    ball: vec (canvasWidth * 0.5 - ballSize * 0.5) (canvasHeight * 0.5 - ballSize * 0.5),
    ballVelocity: vec ballSpeed ballSpeed
  }
  where
    s = canvasHeight / 150.0
    paddleWidth = 6.0 * s
    paddleHeight = 40.0 * s
    ballSize = 6.0 * s
    ballSpeed = 2.0 * s
    paddleY = (canvasHeight * 0.5 - paddleHeight * 0.5)

movePong :: PongInput -> Pong -> Pong
movePong input game =
                  clampBall
                $ ballVsWalls
                $ paddle1VsBall
                $ paddle2VsBall
                $ movePaddles [paddle1Velocity, paddle2Velocity]
                $ moveBall game
  where
    paddle1Velocity = paddleVelocity input.player1 game.paddleSpeed
    paddle2Velocity = paddleVelocity input.player2 game.paddleSpeed

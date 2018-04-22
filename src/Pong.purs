module Pong(init, move) where
import Prelude ((+), (/), (*), (-), negate, pure, bind, Unit, discard, unit, ($), (>>=), otherwise)
import Data.Identity
import Data.Ord (clamp)
import Data.Tuple
import Data.Array

type Vector = {
  x :: Number,
  y :: Number
}

type Input = {
  up :: Boolean,
  down :: Boolean
}

type GameInput = {
  player1 :: Input,
  player2 :: Input
}

paddleVelocity :: Input -> Number -> Number
paddleVelocity {up, down} paddleSpeed
  | up = -paddleSpeed
  | down = paddleSpeed
  | otherwise = 0.0

moveBall :: Game -> Game
moveBall game = game {
    ball {
      x = game.ball.x + game.ballVelocity.x,
      y = game.ball.y + game.ballVelocity.y
    }
  }

movePaddles :: Array Number -> Game -> Game
movePaddles velocities game = game {
    paddles = zipWith movePaddle game.paddles velocities
  }
  where
    movePaddle :: Vector -> Number -> Vector
    movePaddle paddle@{y} velocity = paddle {
        y = clamp 0.0 (game.canvasHeight - game.paddleHeight) (y + velocity)
      }

type Game = {
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

vec :: Number -> Number -> Vector
vec x y = {x, y}

init :: Number -> Number -> Game
init canvasWidth canvasHeight = {
    canvasWidth: canvasWidth,
    canvasHeight: canvasHeight,
    ballSize: 6.0 * s,
    ballSpeed: 2.0 * s,
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
    ball: vec 0.0 0.0,
    ballVelocity: vec 0.0 0.0
  }
  where
    s = canvasHeight / 150.0
    paddleWidth = 6.0 * s
    paddleHeight = 40.0 * s
    paddleY = (canvasHeight * 0.5 - paddleHeight * 0.5)

move :: GameInput -> Game -> Game
move input game = movePaddles [paddle1Velocity, paddle2Velocity]
                $ moveBall game
  where
    paddle1Velocity = paddleVelocity input.player1 game.paddleSpeed
    paddle2Velocity = paddleVelocity input.player2 game.paddleSpeed

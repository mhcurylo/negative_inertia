module Pong(init, move) where
import Prelude ((+), (/), (*), (-), negate, pure, bind, Unit, discard, unit, ($), (>>=), otherwise)
import Data.Identity
import Math (max, min)
import Data.Tuple

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

-- |
-- | Results in third argument clamped within range of first and second
-- | ```purescript
-- | clamp 0.0 1.0 0.5 = 0.5
-- | clamp 0.0 1.0 2.0 = 1.0
-- | clamp 1.0 2.0 0.0 = 1.0
-- | ```
clamp :: Number -> Number -> Number -> Number
clamp minVal maxVal x = min maxVal (max minVal x)

movePaddle1 :: Number -> Game -> Game
movePaddle1 velocity game = game {
    paddle1 {
      y = clamp 0.0 (game.canvasHeight - game.paddleHeight) (game.paddle1.y + velocity)
    }
  }

movePaddle2 :: Number -> Game -> Game
movePaddle2 velocity game = game {
    paddle2 {
      y = clamp 0.0 (game.canvasHeight - game.paddleHeight) (game.paddle2.y + velocity)
    }
  }

type Game = {
  canvasWidth :: Number,
  canvasHeight :: Number,
  ballSize :: Number,
  ballSpeed :: Number,
  paddleWidth :: Number,
  paddleHeight :: Number,
  paddleSpeed :: Number,
  paddle1 :: Vector,
  paddle2 :: Vector,
  score1 :: Number,
  score2 :: Number,
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
    paddle1: vec 0.0 paddleY,
    paddle2: vec (canvasWidth - paddleWidth) paddleY,
    score1: 0.0,
    score2: 0.0,
    ball: vec 0.0 0.0,
    ballVelocity: vec 0.0 0.0
  }
  where
    s = canvasHeight / 150.0
    paddleWidth = 6.0 * s
    paddleHeight = 40.0 * s
    paddleY = (canvasHeight * 0.5 - paddleHeight * 0.5)

move :: GameInput -> Game -> Game
move input game = movePaddle2 paddle2Velocity
                $ movePaddle1 paddle1Velocity
                $ (moveBall game)
  where
    paddle1Velocity = paddleVelocity input.player1 game.paddleSpeed
    paddle2Velocity = paddleVelocity input.player2 game.paddleSpeed

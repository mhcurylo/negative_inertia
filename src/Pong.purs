module Pong(init, move) where
import Prelude ((/), (*), (-))

type Vector = {
  x :: Number,
  y :: Number
}

type Input = {
  up :: Boolean,
  down :: Boolean
}

type Game = {
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

move :: Input -> Input -> Game -> Game
move p1 p2 game = game

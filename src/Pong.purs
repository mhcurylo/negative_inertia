module Pong(init, move) where
import Prelude ((+), (/), (*), (-), negate, pure, bind, Unit, discard, unit, ($), (>>=), otherwise)
import Data.Identity
import Data.Tuple
import Control.Monad.Reader
import Control.Monad.Reader.Trans
import Control.Monad.State

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

type M a = ReaderT GameInput (State Game) a

paddleVelocity :: Input -> Game -> Number
paddleVelocity {up, down} {paddleSpeed}
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

runner :: M Unit
runner = do
  modify moveBall
  game <- get
  input <- ask 
  paddle1Velocity <- pure $ paddleVelocity input.player1 game
  paddle2Velocity <- pure $ paddleVelocity input.player2 game
  modify (\game -> game { paddle1 = vec game.paddle1.x (game.paddle1.y + paddle1Velocity) })
  modify (\game -> game { paddle2 = vec game.paddle2.x (game.paddle2.y + paddle2Velocity) })
  pure unit

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

move :: GameInput -> Game -> Game
move gameInput game = snd $ runState (runReaderT runner gameInput) game

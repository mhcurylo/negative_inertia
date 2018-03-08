module Game (gameLoop, initialGameState) where

import Prelude
import Types 
import Math (abs)
import Collision (collide)
import Data.Maybe
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Array
import Control.Biapply ((<<*>>))
import AABB
import Trace

accBall :: Ball -> Ball
accBall b = b {vel = vec 2.0 0.0}

initialGameState :: GameState
initialGameState = ({
    ball: accBall (createBall 495.0 300.0)
  , paddles: Tuple (createPaddle 30.0 280.0) (createPaddle 960.0 280.0)
  , scores: Tuple 0 0
  , walls: Tuple (createWall 35.0) (createWall 590.0)
})

accUp :: Vector
accUp = vec 0.0 (-1.0)
accDown :: Vector
accDown = vec 0.0 1.0
accStay :: Vector
accStay = zeroVector  

movePlayer :: Move -> Paddle -> Paddle
movePlayer Up p = p {acc = accUp} 
movePlayer Down p = p {acc =  accDown}
movePlayer Stay p = p {acc = accStay }

playerMoves :: PlayerMoves -> GameState -> GameState
playerMoves pms gs@({paddles}) = gs { paddles = (both movePlayer pms <<*>> paddles) }

move :: GameState -> GameState
move gs@{ball, paddles} = gs {ball = ball, paddles = both movePhysical paddles}
 
movePhysical :: Physical -> Physical 
movePhysical p@{pos, vel, acc} = p {
    pos = pos + vel
  , vel = vel + acc
  , acc = acc 
  } 

firstJust :: forall a . Array (Maybe a) -> Maybe a
firstJust = join <<< head <<< filter isJust 

deflect :: Vector -> Vector -> Vector
deflect vel normal = mulV vel (vec nx ny) 
  where
    p x = abs x > 0.0
    nx = if p (getX normal) then (-1.0) else 1.0
    ny = if p (getY normal) then (-1.0) else 1.0

deflectBall :: Physical -> Collision -> Physical
deflectBall ball {time: time, normal: normal} =
  ball {
    pos = ball.pos + scale time ball.vel,
    vel = deflect ball.vel normal
  }
  where
    deflectedVel = deflect ball.vel normal
    pos = ball.pos + scale time ball.vel + scale (1.0 - time) deflectedVel

moveBall :: Physical -> Physical
moveBall ball = ball {
    pos = ball.pos + ball.vel
  }

pong :: GameState -> GameState
pong gs = gs {
    ball = r
  }
  where
    col1 = sweepPhysicals gs.ball (fst gs.paddles)
    col2 = sweepPhysicals gs.ball (snd gs.paddles)
    r = maybe (maybe (moveBall gs.ball) (deflectBall gs.ball) col2) (deflectBall gs.ball) col1

score :: GameState -> GameState
score gs@{ball, scores: (Tuple p1 p2)} = if bx < 0.0
  then initialGameState {scores = Tuple p1 (p2 + 1)}
  else if bx > 1000.0
    then initialGameState {scores = Tuple (p1 + 1) p2}
    else gs
  where
  bx = getX ball.pos      

gameLoop :: PlayerMoves -> GameState -> GameState
gameLoop pm = score <<< pong <<< move <<< playerMoves pm

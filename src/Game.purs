module Game (gameLoop, initialGameState) where

import Prelude
import Types 
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Maybe (Maybe(..), isJust)
import Trace
import AABB
import Control.Biapply ((<<*>>))
-- import Debug.Trace

accBall :: Ball -> Ball
accBall b = b {vel = vec 0.0 2.0}

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

accelerate :: Physical -> Physical 
accelerate p@{pos, vel, acc, inertia} = p {
    pos = pos + vel
  , vel = vel + acc - (vel * inertia) 
  , acc = acc
  } 

playerMoves :: PlayerMoves -> GameState -> GameState
playerMoves pms gs@({paddles}) = gs { paddles = (both movePlayer pms <<*>> paddles) }

move :: GameState -> GameState
move gs@{ball, paddles} = gs {ball = accelerate ball, paddles = both accelerate paddles}

collideWithWalls :: GameState -> GameState
collideWithWalls x = x {
    ball = x.ball {
      pos = case intersection of
              Just b -> x.ball.pos
              Nothing -> x.ball.pos
      ,
      vel = if isJust intersection then vec 0.0 0.0 else x.ball.vel
    }
  }
  where
    ball = fromPhysical $ x.ball
    wall = fromPhysical $ snd x.walls
    intersection = intersectAABBtoAABB ball wall

pongTheBall :: GameState -> GameState
pongTheBall x = x

score :: GameState -> GameState
score x = x

gameLoop :: PlayerMoves -> GameState -> GameState
gameLoop pm = score <<< pongTheBall <<< collideWithWalls <<< move <<< playerMoves pm

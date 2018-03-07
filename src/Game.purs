module Game (gameLoop, initialGameState) where

import Prelude
import Types 
import Math (abs)
import Collision (collide)
import Data.Maybe (Maybe(..), isJust, fromJust, fromMaybe)
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
move gs@{ball, paddles} = gs {ball = accelerate ball, paddles = both accelerate paddles}
 
accelerate :: Physical -> Physical 
accelerate p@{pos, vel, acc, inertia} = p {
    pos = pos + vel
  , vel = vel + acc - (vel * inertia) 
  , acc = acc 
  } 

firstJust :: forall a . Array (Maybe a) -> Maybe a
firstJust = join <<< head <<< filter isJust 

go :: forall a. (a -> a -> Tuple a a) -> a -> Array a -> Array a
go f x s = case uncons s of 
             Just {head: y, tail: xs} ->
               case f x y of
                 Tuple xx yy -> y : (go f x xs)
             Nothing -> s

pong :: GameState -> GameState
pong gs = r
  where
    physicals = [
      gs.ball,
      fst gs.paddles,
      snd gs.paddles,
      fst gs.walls,
      snd gs.walls
    ]
    
    rightPaddle = snd gs.paddles
    xx = fromPhysical gs.ball
    yy = fromPhysical rightPaddle
    intersection = sweepPhysicals gs.ball rightPaddle
    _ = trace intersection
    r = if isJust intersection
        then gs {
          ball = gs.ball {
            vel = vec 0.0 0.0
          }
        }
        else gs

pongx :: GameState -> GameState
pongx gs@{ball, paddles: (Tuple p1 p2), walls: (Tuple w1 w2)} = gs {
       ball = fromMaybe ball $ fst <$> firstJust (collide ball <$> [p1, p2, w1, w2])
     , paddles = Tuple (collideWalls p1) (collideWalls p2) 
  }
  where
  collideWalls pad = fromMaybe pad $ fst <$> firstJust (collide pad <$> [w1, w2])      

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

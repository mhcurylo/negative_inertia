module Types where

import Prelude 
import Data.Tuple (Tuple)
import Data.Number (infinity)
import Data.Bifunctor (bimap) 
import Vector (Vector, vec, origin)

data Move = Up | Down | Stay

data PhysicalKind = Ball | Wall | Paddle

type Physical = {
    pos  :: Vector
  , vel  :: Vector
  , acc  :: Vector
  , size :: Vector
  , inertia :: Vector
  , kind :: PhysicalKind
  , mass :: Number
}

showPhysical :: Physical -> String
showPhysical ({pos, size, vel}) = "Physical pos: " <> show pos <> ", size: " <> show size <> ", vel: " <> show vel <> ";"

type Double a = Tuple a a
type Score = Int
type Paddle = Physical 
type Ball = Physical 
type Wall = Physical 

both :: forall a b . (a -> b) -> Double a -> Double b 
both f = bimap f f

type Paddles = Double Paddle 
type Scores = Double Score
type Walls = Double Wall
type PlayerMoves = Double Move 

type GameState = {
      ball :: Ball
    , paddles :: Paddles  
    , walls :: Walls
    , scores :: Scores
}

data Game = Start
  | Progress GameState
  | Finish Int Int 
  | Suspend Int Game

createPhysical :: PhysicalKind -> Number -> Number -> Number -> Number -> Number -> Number -> Physical
createPhysical kind ix iy w h x y = ({
    pos: vec x y
  , acc: origin
  , vel: origin
  , size: vec w h
  , inertia: vec ix iy
  , kind: kind
  , mass: infinity
}) 

createBox :: Number -> Number -> Number -> Number -> Wall
createBox = createPhysical Wall 0.0 0.0

createPaddle :: Number -> Number -> Paddle
createPaddle = createPhysical Paddle 0.0 0.90 20.0 70.0

createBall :: Number -> Number -> Ball
createBall = createPhysical Ball 1.001 1.001 15.0 15.0

createWall :: Number -> Wall
createWall = createBox 960.0 5.0 20.0

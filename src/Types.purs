module Types where

import Data.Tuple (Tuple)
import Data.Bifunctor (bimap) 

data Move = Up | Down | Stay

type Vector = {
    x :: Number
  , y :: Number
}

type Physical = {
    pos  :: Vector 
  , vel  :: Vector
  , acc  :: Vector
  , size :: Vector
  , inertia :: Number
}

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




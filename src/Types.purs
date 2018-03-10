module Types where

import Prelude 
import Data.Tuple (Tuple)
import Data.Bifunctor (bimap) 
import Data.Generic (class Generic)

data Move = Up | Down | Stay

data Vector = Vector {
    x :: Number
  , y :: Number
}

derive instance genericVector :: Generic Vector

addV :: Vector -> Vector -> Vector
addV (Vector a) (Vector b) = vec (a.x + b.x) (a.y + b.y)

subV :: Vector -> Vector -> Vector
subV (Vector a) (Vector b) = vec (a.x - b.x) (a.y - b.y)

mulV :: Vector -> Vector -> Vector
mulV (Vector a) (Vector b) = vec (a.x * b.x) (a.y * b.y)

vec :: Number -> Number -> Vector
vec x y = Vector ({x, y})  

getX :: Vector -> Number
getX (Vector {x}) = x

getY :: Vector -> Number
getY (Vector {y}) = y

setY :: Number -> Vector -> Vector
setY n (Vector {x,y}) = Vector {x,y: n}  

setX :: Number -> Vector -> Vector
setX n (Vector {x,y}) = Vector {x: n,y}  

scale :: Number -> Vector -> Vector
scale s (Vector {x,y}) = vec (s * x) (s * y)

zeroVector :: Vector
zeroVector = vec 0.0 0.0

oneVector :: Vector
oneVector = vec 1.0 1.0  

instance eqVector :: Eq Vector where
  eq (Vector v1) (Vector v2) = v1.x == v2.x && v1.y == v2.y

instance showVector :: Show Vector where
  show (Vector {x, y}) = "Vector x: " <> show x <> ", y: " <> show y <> ";" 

instance semiringVector :: Semiring Vector where
  add = addV
  mul = mulV
  one = oneVector
  zero = zeroVector

instance ringVector :: Ring Vector where
  sub = subV

type Physical = {
    name :: String
  , pos  :: Vector 
  , vel  :: Vector
  , acc  :: Vector
  , size :: Vector
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

createPhysical :: String -> Number -> Number -> Number -> Number -> Physical
createPhysical name w h x y = ({
    name: name
  , pos: vec x y
  , acc: zeroVector
  , vel: zeroVector
  , size: vec w h
}) 

createBox :: String -> Number -> Number -> Number -> Number -> Physical
createBox = createPhysical

createPaddle :: String -> Number -> Number -> Paddle
createPaddle name = createPhysical name 20.0 70.0

createBall :: String -> Number -> Number -> Ball
createBall name = createPhysical name 15.0 15.0

createWall :: String -> Number -> Wall
createWall name = createBox name 960.0 5.0 20.0

module Types where

import Prelude 
import Data.Tuple (Tuple)
import Data.Bifunctor (bimap) 

data Move = Up | Down | Stay

data Vector = Vector {
    x :: Number
  , y :: Number
}

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

zeroVector :: Vector
zeroVector = vec 0.0 0.0

oneVector :: Vector
oneVector = vec 1.0 1.0  

instance eqVector :: Eq Vector where
  eq (Vector v1) (Vector v2) = v1.x == v2.x && v1.y == v2.y

instance showVector :: Show Vector where
  show (Vector {x, y}) = "Vector x: " <> show x <> ", y: " <> show y <> "." 

instance semiringVector :: Semiring Vector where
  add = addV
  mul = mulV
  one = oneVector
  zero = zeroVector

instance ringVector :: Ring Vector where
  sub = subV


type Physical = {
    pos  :: Vector 
  , vel  :: Vector
  , acc  :: Vector
  , size :: Vector
  , inertia :: Vector
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


createPhysical :: Number -> Number -> Number -> Number -> Number -> Physical
createPhysical i w h x y = ({
    pos: vec x y
  , acc: zeroVector
  , vel: zeroVector
  , size: vec w h
  , inertia: vec i i
}) 

createBox :: Number -> Number -> Number -> Number -> Physical
createBox = createPhysical 0.0

createPaddle :: Number -> Number -> Paddle
createPaddle = createPhysical 0.1 10.0 50.0

createBall :: Number -> Number -> Ball
createBall = createPhysical (-0.001) 5.0 5.0 

createWall :: Number -> Wall
createWall = createBox 960.0 5.0 20.0


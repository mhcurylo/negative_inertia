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

dot :: Vector -> Vector -> Number
dot (Vector {x: x1, y: y1}) (Vector {x: x2, y: y2}) = x1 * x2 + y1 * y2

opposite :: Vector -> Vector
opposite = (*) (scale (-1.0) oneVector)

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

data PhysicalKind = Ball | Wall | Paddle

type Physical = {
    pos  :: Vector
  , vel  :: Vector
  , acc  :: Vector
  , size :: Vector
  , inertia :: Vector
  , kind :: PhysicalKind
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

createPhysical :: PhysicalKind -> Number -> Number -> Number -> Number -> Number -> Number -> Physical
createPhysical kind ix iy w h x y = ({
    pos: vec x y
  , acc: zeroVector
  , vel: zeroVector
  , size: vec w h
  , inertia: vec ix iy
  , kind: kind
}) 

createPaddle :: Number -> Number -> Paddle
createPaddle = createPhysical Paddle 0.0 0.95 20.0 70.0

createBall :: Number -> Number -> Ball
createBall = createPhysical Ball 1.001 1.001 15.0 15.0

createWall :: Number -> Wall
createWall = createPhysical Wall 0.0 0.0 960.0 5.0 20.0

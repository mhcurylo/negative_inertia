module Vector(
  Vector(..),
  vec,
  origin,
  scale,
  dot,
  opposite,
  getX,
  getY,
  setX,
  setY
) where

import Prelude
import Data.Generic (class Generic)

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

origin :: Vector
origin = vec 0.0 0.0

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
  zero = origin

instance ringVector :: Ring Vector where
  sub = subV

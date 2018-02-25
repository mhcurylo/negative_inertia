module Collision (Direction, collide, didCollide, minkowskiDifference, MinkowskiDifference) where

import Prelude 
import Types 
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Foldable (minimumBy)
import Data.Array (zip)
import Control.Biapply ((<<*>>))
import Control.Alt ((<|>))
import Data.Generic (class Generic, gShow)
-- import Debug.Trace

data MinkowskiDifference = MinkowskiDifference {
    mTop :: Vector
  , mSize :: Vector  
  , mBottom :: Vector
}

derive instance genericMinkowskiDifference :: Generic MinkowskiDifference
instance showMinkowskiDifference :: Show MinkowskiDifference where
  show = gShow

data RelativeMotion = RelativeMotion {
    source :: Vector
  , direction :: Vector  
}

derive instance genericRelativeMotion :: Generic RelativeMotion
instance showRelativeMotion :: Show RelativeMotion where
  show = gShow

data Direction = T | R | B | L
derive instance genericDirection :: Generic Direction
instance showDirection :: Show Direction where
  show = gShow

collide :: Physical -> Physical -> Maybe (Tuple Physical Physical)
collide a b = collision a b <$> didCollide a b

ycVec :: Vector
ycVec = vec 1.0 (-1.0)
xcVec :: Vector
xcVec = vec (-1.0) 1.0

collision :: Physical -> Physical -> Direction -> Tuple Physical Physical
collision a b T = Tuple a' b' 
  where
  a' = a {vel = a.vel * ycVec }      
--  a' = a {vel = a.vel * ycVec, pos = setY (getY b.pos + getY b.size + 0.1) a.pos}      
  b' = b {vel = b.vel * ycVec }      
collision a b B = Tuple a' b' 
  where
  a' = a {vel = a.vel * ycVec }      
-- a' = a {vel = a.vel * ycVec, pos = setY (getY b.pos - getY a.size - 0.1) a.pos}      
  b' = b {vel = b.vel * ycVec }      
collision a b R = Tuple a' b' 
  where
  a' = a {vel = a.vel * xcVec + b.vel }      
  b' = b {vel = b.vel * xcVec }      
collision a b L = Tuple a' b' 
  where
  a' = a {vel = a.vel * xcVec + b.vel }      
  b' = b {vel = b.vel * xcVec }      

didCollide :: Physical -> Physical -> Maybe Direction
didCollide a b = if maxX >= 0.0  
                   && maxY >= 0.0
                   && minY <= 0.0 
                   && minX <= 0.0 
  then snd <$> (minimumBy (comparing fst) $ zip [maxY, maxX, (-minY), (-minX)] [T, R, B, L])
  else Nothing
  where
  (MinkowskiDifference {mTop, mBottom}) = minkowskiDifference a b      
  minX = min (getX mTop) (getX mBottom) 
  maxX = max (getX mTop) (getX mBottom)
  minY = min (getY mBottom) (getY mTop)
  maxY = max (getY mBottom) (getY mTop)

center :: Physical -> Vector
center ({pos, size}) = (pos + size) * (vec 0.5 0.5)

relativeMotion :: Physical -> Physical -> RelativeMotion
relativeMotion a b = RelativeMotion {source, direction}
  where
  source = center a 
  direction = subV a.vel b.vel

minkowskiDifference :: Physical -> Physical -> MinkowskiDifference
minkowskiDifference a b = MinkowskiDifference {mTop, mSize, mBottom}
  where
  mTop = a.pos - b.pos - b.size
  mSize = a.size + b.size
  mBottom = mTop + mSize 

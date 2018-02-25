module Collision (Direction, didCollide) where

import Prelude 
import Types 
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Control.Biapply ((<<*>>))
import Control.Alt ((<|>))
-- import Debug.Trace

data MinkowskiDifference = MinkowskiDifference {
    mTop :: Vector
  , mSize :: Vector  
  , mBottom :: Vector
}

data RelativeMotion = RelativeMotion {
    source :: Vector
  , direction :: Vector  
}

data Direction = L | R | T | B

didCollide :: Physical -> Physical -> Maybe Direction
didCollide a b = if getX mTop <= 0.0  
                   && getY mTop >= 0.0
                   && getX mBottom >= 0.0 
                   && getY mBottom <= 0.0 
  then Just L 
  else Nothing
  where
  (MinkowskiDifference {mTop, mBottom}) = minkowskiDifference a b      


center :: Physical -> Vector
center ({pos, size}) = (pos + size) * (vec 0.5 0.5)

relativeMotion :: Physical -> Physical -> RelativeMotion
relativeMotion a b = RelativeMotion {source, direction}
  where
  source = subV a.vel b.vel
  direction = center a 

minkowskiDifference :: Physical -> Physical -> MinkowskiDifference
minkowskiDifference a b = MinkowskiDifference {mTop, mSize, mBottom}
  where
  mTop = subV a.pos (addV b.pos b.size)
  mSize = addV a.size b.size
  mBottom = addV mTop mSize 

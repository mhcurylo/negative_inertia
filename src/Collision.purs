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

data Direction = T | R | B | L

didCollide :: Physical -> Physical -> Maybe Direction
didCollide a b = if tx <= 0.0  
                   && ty >= 0.0
                   && bx >= 0.0 
                   && by <= 0.0 
  then Just $ minimumBy  $ zip [ty, bx, by, ty] [T, R, B, L] 
  else Nothing
  where
  (MinkowskiDifference {mTop, mBottom}) = minkowskiDifference a b      
  ty = getX mTop 
  tx = getY mTop
  bx = getX mBottom 
  by = getY mBottom  


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
  mTop = a.pos - (b.pos + b.size)
  mSize = a.size + b.size
  mBottom = mTop + mSize 

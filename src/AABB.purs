module AABB(AABB, fromPhysical, intersectAABBtoAABB) where

import Prelude
import Types (Physical, getX, getY)
import Data.Maybe (Maybe(..))

type AABB = {
  top :: Number,
  right :: Number,
  bottom :: Number,
  left :: Number
}

fromPhysical :: Physical -> AABB
fromPhysical x = {  
    top: getY x.pos,
    right: getX x.pos + getX x.size,
    bottom: getY x.pos + getY x.size,
    left: getX x.pos
  }

-- | Separating Axis Theorem to find overlaping AABB of two AABB's
intersectAABBtoAABB x y
  | x.top > y.bottom = Nothing
  | x.bottom < y.top = Nothing
  | x.left > y.right = Nothing
  | x.right < y.left = Nothing
  | otherwise = Just {
      top: if x.top >= y.top && x.top <= y.bottom then x.top else y.top,
      right: if x.right >= y.left && x.right <= y.right then x.right else y.right,
      bottom: if x.bottom >= y.top && x.bottom <= y.bottom then x.bottom else y.bottom,
      left: if x.left >= y.left && x.left <= y.right then x.left else y.left
    }

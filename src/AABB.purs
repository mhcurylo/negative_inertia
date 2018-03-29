module AABB(AABB, fromPhysical, sweepAABB, Collision, sweepPhysicals) where

import Prelude ((||), (&&), (>=), (<=), (>), (<), (-), (+), (/), (==), otherwise, negate)
import Data.Number (infinity)
import Math (max, min)
import Types (Physical)
import Vector (Vector(Vector), getX, getY, vec)
import Data.Maybe (Maybe(..))

type Collision = {
  time :: Number,
  normal :: Vector
}

type AABB = {
  top :: Number,
  right :: Number,
  bottom :: Number,
  left :: Number
}

-- | Gets AABB that wraps a Physical
fromPhysical :: Physical -> AABB
fromPhysical x = {  
    top: getY x.pos,
    right: getX x.pos + getX x.size,
    bottom: getY x.pos + getY x.size,
    left: getX x.pos
  }

-- | Separating Axis Theorem to find overlaping AABB of two AABB's
intersectAABBtoAABB :: AABB -> AABB -> Maybe AABB
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

-- | Test two "physicals" for collision
sweepPhysicals :: Physical -> Physical -> Maybe Collision
sweepPhysicals a b = sweepAABB v (fromPhysical a) (fromPhysical b)
  where
    v = a.vel - b.vel

-- | Find intersection time (0..1 inclusive) of moving AABB to static AABB
-- | First argument is velocity of first AABB
-- |
-- | Based on https://www.gamedev.net/articles/programming/general-and-gameplay-programming/swept-aabb-collision-detection-and-response-r3084/
-- |
sweepAABB :: Vector -> AABB -> AABB -> Maybe Collision
sweepAABB (Vector {x: vx, y: vy}) a b
  | vx == 0.0 && (a.right < b.left || a.left > b.right) = Nothing
  | vy == 0.0 && (a.bottom < b.top || a.top > b.bottom) = Nothing
  | otherwise = r
  where
    xInvEntry = if vx > 0.0 then b.left - a.right else b.right - a.left
    xInvExit = if vx > 0.0 then b.right - a.left else b.left - a.right
    yInvEntry = if vy > 0.0 then b.top - a.bottom else b.bottom - a.top
    yInvExit = if vy > 0.0 then b.bottom - a.top else b.top - a.bottom
    xEntry = if vx == 0.0 then -infinity else xInvEntry / vx
    xExit = if vx == 0.0 then infinity else xInvExit / vx
    yEntry = if vy == 0.0 then -infinity else yInvEntry / vy
    yExit = if vy == 0.0 then infinity else yInvExit / vy
    entryTime = max xEntry yEntry
    exitTime = min xExit yExit
    nx = if xInvEntry < 0.0 then 1.0 else -1.0
    ny = if yInvEntry < 0.0 then 1.0 else -1.0
    normal = if xEntry > yEntry then vec nx 0.0 else if xEntry < yEntry then vec 0.0 ny else vec nx ny
    r = if entryTime > exitTime || xEntry < 0.0 && yEntry < 0.0 || xEntry > 1.0 || yEntry > 1.0
        then Nothing
        else Just {
          time: entryTime,
          normal: normal
        }

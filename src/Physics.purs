module Physics(simulate) where
import Prelude ((<$>), ($), (>), (<), (>=), (-), (+), negate)
import Math(abs)
import AABB (Collision, sweepPhysicals)
import Algorithm (foldPairs)
import Types(Vector, Physical, mulV, getX, getY, vec, scale, oneVector)
import Data.Array (modifyAt, mapWithIndex)
import Data.Tuple (Tuple(Tuple))
import Data.Maybe (Maybe(..), fromMaybe)

movePhysical :: Number -> Physical -> Physical
movePhysical time p@{pos, vel, acc} = p {
    pos = pos + scale time vel
  , vel = vel + scale time acc
  , acc = acc
  }

-- | Find a pair of indices of earliest colliding elements in array
firstCollision :: Array Physical -> Maybe { collision :: Collision, i :: Int, j :: Int }
firstCollision v = foldPairs f Nothing (mapWithIndex Tuple v)
  where
    f z (Tuple i x) (Tuple j y) = case sweepPhysicals x y of
      Just collision -> case z of
        Just zz -> if zz.collision.time < collision.time
                   then z
                   else Just {collision, i, j}
        Nothing -> Just {collision, i, j}
      Nothing -> z

deflect :: Vector -> Vector -> Vector
deflect vel normal = mulV vel (vec nx ny) 
  where
    p x = abs x > 0.0
    nx = if p (getX normal) then (-1.0) else 1.0
    ny = if p (getY normal) then (-1.0) else 1.0

-- | Return a physical with reflected velocity for collision
deflectPhysical :: Collision -> Physical -> Physical
deflectPhysical {normal} x = x { vel = deflect x.vel normal }

-- | Return a collision with opposite normal
opposite :: Vector -> Vector
opposite = mulV (scale (-1.0) oneVector)

-- | Simulate an array of physicals over time
-- | Recurses for as long as there are collisions
simulate :: Number -> Array Physical -> Array Physical
simulate time v =
  case firstCollision v of
    Just {collision, i, j} ->
      if collision.time >= time
      then justMove
      else simulate (time - collision.time) (u2 collision i j (movePhysical collision.time <$> v))
    Nothing -> justMove
  where
    justMove = movePhysical time <$> v
    u c i w = fromMaybe w (modifyAt i (deflectPhysical c) w)
    u2 c i j w = u (c {normal = opposite c.normal}) i $ u c j w

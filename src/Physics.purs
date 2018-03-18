module Physics(simulate) where
import Prelude ((<$>), ($), (>), (<), (>=), (-), (+), negate)
import Math(abs)
import AABB (Collision, sweepPhysicals)
import Algorithm (foldPairs, listTuple)
import Types(Vector(Vector), Physical, mulV, getX, getY, vec, scale)
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
                   else Just {collision: collision, i: i, j: j}
        Nothing -> Just {collision: collision, i: i, j: j}
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
opposite :: Collision -> Collision
opposite x@{normal} = x {normal = mulV (vec (-1.0) (-1.0)) normal}

-- | Simulate an array of physicals over time
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
    u c i v = fromMaybe v (modifyAt i (deflectPhysical c) v)
    u2 c i j v = u (opposite c) j $ u c i v

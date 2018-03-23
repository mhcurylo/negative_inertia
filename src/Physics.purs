module Physics(simulate) where
import Prelude ((<$>), ($), (>), (<), (>=), (-), (+), negate, (*))
import Math (abs)
import AABB (Collision, sweepPhysicals)
import Algorithm (foldPairs)
import Types(Vector, Physical, getX, getY, vec, scale, oneVector, both)
import Data.Array (modifyAt, updateAt, mapWithIndex, index)
import Data.Tuple (Tuple(Tuple))
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Partial.Unsafe (unsafePartial)

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
deflect vel normal = vel * vec nx ny
  where
    p x = abs x > 0.0
    nx = if p (getX normal) then (-1.0) else 1.0
    ny = if p (getY normal) then (-1.0) else 1.0

-- | Return a physical with reflected velocity for collision
deflectPhysical :: Collision -> Physical -> Physical
deflectPhysical {normal} x = x { vel = deflect x.vel normal }

collide :: Collision -> Physical -> Physical -> Tuple Physical Physical
collide collision@{normal} a b = Tuple a' b'
  where
    a' = deflectPhysical collision a
    b' = deflectPhysical collision {normal = opposite normal} b

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex v i = unsafePartial $ fromJust $ index v i

unsafeUpdateAt :: forall a. Int -> a -> Array a -> Array a
unsafeUpdateAt i x v = unsafePartial $ fromJust $ updateAt i x v

unsafeUpdateBothAt :: forall a. (a -> a -> Tuple a a) -> Int -> Int -> Array a -> Array a
unsafeUpdateBothAt f i j v =
  let
    Tuple x' y' = f (unsafeIndex v i) (unsafeIndex v j)
  in
    unsafeUpdateAt j y' (unsafeUpdateAt i x' v)

-- | Return a collision with opposite normal
opposite :: Vector -> Vector
opposite = (*) (scale (-1.0) oneVector)

-- | Simulate an array of physicals over time
-- | Recurses for as long as there are collisions
simulate :: Number -> Array Physical -> Array Physical
simulate time v =
  case firstCollision v of
    Just {collision, i, j} ->
      if collision.time >= time
      then justMove
      else simulate (time - collision.time)
                    (unsafeUpdateBothAt (collide collision) i j (movePhysical collision.time <$> v))
    Nothing -> justMove
  where
    justMove = movePhysical time <$> v

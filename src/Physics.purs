module Physics (
  simulate,
  defaultCollide,
  CollisionHandler,
  composeCollisions
) where
import Prelude ((<$>), ($), (<), (>=), (-), (+), negate, (*), (/))
import Math (max)
import AABB (Collision, sweepPhysicals)
import Algorithm (foldPairs)
import Types (Physical)
import Vector (scale, dot, opposite)
import Data.Array (updateAt, mapWithIndex, index)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

type CollisionHandler = Collision -> Physical -> Physical -> Tuple Physical Physical

composeCollisions :: CollisionHandler -> CollisionHandler -> CollisionHandler
composeCollisions f g c x y = uncurry (f c) (g c x y)

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

-- | Apply collision impulse
-- |
-- | Reference https://gafferongames.com/post/collision_response_and_coulomb_friction/
applyLinearCollisionImpulse :: Collision -> Physical -> Physical
applyLinearCollisionImpulse {normal} x@{vel} =
  let
    restitution = 1.0
    d = dot vel normal
    j = max (-(1.0 + restitution) * d) 0.0
  in
    x { vel = vel + scale j normal }

-- | Calculate impulse response of first physical to second
-- | XXX: function name is not accurate
-- |
-- | Scalar formula
-- | console.warn( ((m2 * (v2 ** 2)) / m1) ** 0.5)
-- |
applyIR :: Physical -> Physical -> Physical
applyIR p1 p2 = p1 { vel = r }
  where
    v1 = p1.vel
    m1 = p1.mass
    v2 = p2.vel
    m2 = p2.mass
    r = scale (1.0 / m1) (scale m2 v2)

defaultCollide :: CollisionHandler
defaultCollide collision@{normal} a b = Tuple a' b'
  where
    a' = applyLinearCollisionImpulse collision a
    b' = applyLinearCollisionImpulse collision {normal = opposite normal} b

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

-- | Simulate an array of physicals over time
-- | Recurses for as long as there are collisions
simulate :: CollisionHandler -> Number -> Array Physical -> Array Physical
simulate collide time v =
  case firstCollision v of
    Just {collision, i, j} ->
      if collision.time >= time
      then justMove
      else
          let
            updated = (unsafeUpdateBothAt (collide collision) i j (movePhysical collision.time <$> v))
          in
            simulate collide (time - collision.time) updated
    Nothing -> justMove
  where
    justMove = movePhysical time <$> v

module Test.AABB(testAABB) where

import Control.Monad.Eff.Random (RANDOM)
import Test.Unit (test, TestF)
import Control.Monad.Free (Free)
import Test.Unit.QuickCheck (quickCheck)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import AABB (AABB, Collision, sweepPhysicals, sweepAABB)
import Types (createBox, vec, Vector, Physical)
import Prelude (Unit, discard, negate, ($), (&&), (+), (==))

sameCollision :: Maybe Collision -> Maybe Collision -> Boolean
sameCollision (Just a) (Just b) = a.time == b.time && a.normal == b.normal
sameCollision Nothing Nothing = true
sameCollision Nothing _ = false
sameCollision _ Nothing = false

qc :: forall a. Maybe Collision -> Maybe Collision -> Aff ( random :: RANDOM | a) Unit
qc x y = quickCheck $ sameCollision x y

fromRect :: Number -> Number -> Number -> Number -> AABB
fromRect x y w h = {top: y, right: x + w, bottom: y + h, left: x}

setVel :: Vector -> Physical -> Physical
setVel v p = p { vel = v }

testAABB :: forall a. Free (TestF ( random :: RANDOM | a)) Unit
testAABB = do
  test "sweepPhysicals" do
    qc Nothing (sweepPhysicals (createBox 1.0 1.0 0.0 0.0) (createBox 1.0 1.0 2.0 0.0))
    qc (Just {time: 0.25, normal: vec (-1.0) 0.0})
       (sweepPhysicals (setVel (vec 4.0 0.0) (createBox 1.0 1.0 0.0 0.0))
                       (createBox 1.0 1.0 2.0 0.0))

  test "sweepAABB" do
    qc Nothing (sweepAABB (vec 0.0 0.0) (fromRect 0.0 0.0 1.0 1.0) (fromRect 1.0 0.0 1.0 1.0))
    qc (Just {time: 0.25, normal: vec (-1.0) 0.0})    (sweepAABB (vec 4.0 0.0)       (fromRect 0.0 0.0 1.0 1.0) (fromRect 2.0 0.0 1.0 1.0))
    qc (Just {time: 0.25, normal: vec 0.0 (-1.0)})    (sweepAABB (vec 0.0 4.0)       (fromRect 0.0 0.0 1.0 1.0) (fromRect 0.0 2.0 1.0 1.0))
    qc (Just {time: 1.00, normal: vec 1.0 0.0})       (sweepAABB (vec (-1.0) 0.0)    (fromRect 2.0 2.0 1.0 1.0) (fromRect 0.0 0.0 1.0 1.0))
    qc (Just {time: 1.00, normal: vec 0.0 1.0})       (sweepAABB (vec 0.0 (-1.0))    (fromRect 2.0 2.0 1.0 1.0) (fromRect 0.0 0.0 1.0 1.0))
    qc (Just {time: 0.50, normal: vec (-1.0) (-1.0)}) (sweepAABB (vec 2.0 2.0)       (fromRect 0.0 0.0 1.0 1.0) (fromRect 2.0 2.0 1.0 1.0))
    qc (Just {time: 0.50, normal: vec (1.0) (1.0)})   (sweepAABB (vec (-2.0) (-2.0)) (fromRect 2.0 2.0 1.0 1.0) (fromRect 0.0 0.0 1.0 1.0))

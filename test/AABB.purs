module Test.AABB(testAABB) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.AVar (AVAR)
import Test.Unit (test, TestF)
import Test.Unit.Main (runTest)
import Control.Monad.Free (Free)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(), (===))
import Control.Monad.Aff (runAff, Aff)
import Data.Maybe
import Data.Boolean
import AABB
import Types
import Prelude

sameCollision :: Maybe Collision -> Maybe Collision -> Boolean
sameCollision (Just a) (Just b) = a.time == b.time && a.normal == b.normal
sameCollision Nothing Nothing = true
sameCollision Nothing _ = false
sameCollision _ Nothing = false

qc :: forall a. Maybe Collision -> Maybe Collision -> Aff ( random :: RANDOM | a) Unit
qc x y = quickCheck $ sameCollision x y

fromRect :: Number -> Number -> Number -> Number -> AABB
fromRect x y w h = {top: y, right: x + w, bottom: y + h, left: x}

testAABB :: forall a. Free (TestF ( random :: RANDOM | a)) Unit
testAABB = do
  test "sweepAABB" do
    qc Nothing (sweepAABB (vec 0.0 0.0) (fromRect 0.0 0.0 1.0 1.0) (fromRect 1.0 0.0 1.0 1.0))
    qc (Just {time: 0.25, normal: vec (-1.0) 0.0})    (sweepAABB (vec 4.0 0.0)       (fromRect 0.0 0.0 1.0 1.0) (fromRect 2.0 0.0 1.0 1.0))
    qc (Just {time: 0.25, normal: vec 0.0 (-1.0)})    (sweepAABB (vec 0.0 4.0)       (fromRect 0.0 0.0 1.0 1.0) (fromRect 0.0 2.0 1.0 1.0))
    qc (Just {time: 1.00, normal: vec 1.0 0.0})       (sweepAABB (vec (-1.0) 0.0)    (fromRect 2.0 2.0 1.0 1.0) (fromRect 0.0 0.0 1.0 1.0))
    qc (Just {time: 1.00, normal: vec 0.0 1.0})       (sweepAABB (vec 0.0 (-1.0))    (fromRect 2.0 2.0 1.0 1.0) (fromRect 0.0 0.0 1.0 1.0))
    qc (Just {time: 0.50, normal: vec (-1.0) (-1.0)}) (sweepAABB (vec 2.0 2.0)       (fromRect 0.0 0.0 1.0 1.0) (fromRect 2.0 2.0 1.0 1.0))
    qc (Just {time: 0.50, normal: vec (1.0) (1.0)})   (sweepAABB (vec (-2.0) (-2.0)) (fromRect 2.0 2.0 1.0 1.0) (fromRect 0.0 0.0 1.0 1.0))

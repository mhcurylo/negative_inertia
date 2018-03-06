module Test.AABB(testAABB) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.AVar (AVAR)
import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.QuickCheck (quickCheck)

testAABB = do
  test "aabb" do
    quickCheck (1.0 == 1.0)


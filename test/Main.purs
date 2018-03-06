module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.AVar (AVAR)
import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.QuickCheck (quickCheck)

import Test.QuickCheck (Result(), (===))
import Test.AABB (testAABB)

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

main :: forall t8. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, random :: RANDOM | t8) Unit 
main = runTest do
  test "the commutative property" do
    quickCheck theCommutativeProperty
  testAABB

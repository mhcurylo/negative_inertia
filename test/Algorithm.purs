module Test.Algorithm where

import Prelude

import Control.Monad.Free (Free)
import Test.Unit (test, TestF)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit.QuickCheck (quickCheck)

import Test.QuickCheck ((===))
import Data.Tuple (Tuple(..))
import Algorithm (foldPairs, listTuple)

testAlgorithm :: forall a. Free (TestF ( random :: RANDOM | a)) Unit
testAlgorithm = do
  test "foldrPairs" do
    quickCheck ("_,1+2,1+3,1+4,2+3,2+4,3+4" === foldPairs (\z x y-> z <> "," <> x <> "+" <> y) "_" ["1","2","3","4"])
  test "listTuple" do
    quickCheck ([1, 2] === listTuple (Tuple 1 2))
    quickCheck ([2, 1] === listTuple (Tuple 2 1))


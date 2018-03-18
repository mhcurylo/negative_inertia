module Test.Algorithm where

import Prelude

import Control.Monad.Free (Free)
import Test.Unit (test, TestF)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit.QuickCheck (quickCheck)

import Test.QuickCheck ((===))
import Data.Tuple (Tuple(..))
import Algorithm (foldPairs, listTuple)

foldPairsFunc :: String -> String -> String -> String
foldPairsFunc z x y = z <> "," <> x <> "+" <> y

testAlgorithm :: forall a. Free (TestF ( random :: RANDOM | a)) Unit
testAlgorithm = do
  test "foldrPairs" do
    quickCheck ("_,1+2,1+3,1+4,2+3,2+4,3+4" === foldPairs foldPairsFunc "_" ["1","2","3","4"])
    quickCheck ("_,1+2" === foldPairs foldPairsFunc "_" ["1","2"])
    quickCheck ("_" === foldPairs foldPairsFunc "_" [])
  test "listTuple" do
    quickCheck ([1, 2] === listTuple (Tuple 1 2))
    quickCheck ([2, 1] === listTuple (Tuple 2 1))


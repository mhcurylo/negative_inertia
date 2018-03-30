module Test.Algorithm where

import Prelude

import Algorithm (foldPairs, listTuple)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Free (Free)
import Data.Tuple (Tuple(..))
import Test.QuickCheck (QC, (===))
import Test.Unit (test, TestF)
import Test.Unit.QuickCheck (quickCheck)

foldPairsFunc :: String -> String -> String -> String
foldPairsFunc z x y = z <> "," <> x <> "+" <> y

listsTuple :: Int -> Int -> Boolean
listsTuple x y = [x, y] == listTuple (Tuple x y) 

testAlgorithm :: forall a. Free (TestF ( random :: RANDOM | a)) Unit
testAlgorithm = do
  test "foldrPairs" do
    quickCheck ("_,1+2,1+3,1+4,2+3,2+4,3+4" === foldPairs foldPairsFunc "_" ["1","2","3","4"])
    quickCheck ("_,1+2" === foldPairs foldPairsFunc "_" ["1","2"])
    quickCheck ("_" === foldPairs foldPairsFunc "_" [])
  test "listTuple" do
    quickCheck listsTuple 


module Algorithm (foldPairs, listTuple) where
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | Applies function to unique pairs of elements of array
-- | O((n^2)/2)
-- |
-- | ```purescript
-- | foldPairs (\z x y-> z <> "," <> x <> "+" <> y) "_" ["1", "2", "3", "4"]
-- |    = "_,1+2,1+3,1+4,2+3,2+4,3+4"
-- | ```
-- |
foldPairs :: forall a b. (b -> a -> a -> b) -> b -> Array a -> b
foldPairs f z s = case uncons s of
              Just {head: h, tail: t} -> foldPairs f (inner z h t) t
              Nothing -> z
  where
    inner :: b -> a -> Array a -> b
    inner zz x ss = case uncons ss of
                      Just {head: h, tail: t} -> inner (f zz x h) x t
                      Nothing -> zz

-- | Returns an array of elements of homogenous tuple
-- |
-- | ```purescript
-- | listTuple (Tuple 1 2) = [1, 2]
-- | ```
-- |
listTuple :: forall a . Tuple a a -> Array a
listTuple (Tuple x y) = [x, y]

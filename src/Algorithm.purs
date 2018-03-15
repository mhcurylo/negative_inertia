module Algorithm (foldPairs) where
import Data.Array (uncons)
import Data.Maybe (Maybe(..))

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
              Just {head: h, tail: t} -> foldPairs f (inner f z h t) t
              Nothing -> z
  where
    inner :: forall a b. (b -> a -> a -> b) -> b -> a -> Array a -> b
    inner f z x s = case uncons s of
                      Just {head: h, tail: t} -> inner f (f z x h) x t
                      Nothing -> z

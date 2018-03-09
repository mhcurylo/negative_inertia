module Trace(trace) where

foreign import trace :: forall a. a -> a

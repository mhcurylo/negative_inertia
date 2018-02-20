module GameLoop (animateGame) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Set (Set)
import FRP (FRP)
import FRP.Behavior.Keyboard (keys)
import FRP.Behavior (sample_)
import FRP.Event (subscribe, fold)
import FRP.Event.Time (animationFrame)

animateGame :: forall a b c . (Set Int -> b -> b) -> b -> (b -> Eff (frp :: FRP | c) a) -> Eff (frp :: FRP | c) (Eff (frp :: FRP | c) Unit)
animateGame gameLoop init renderer = subscribe (fold gameLoop (sample_ keys animationFrame) init) renderer 

module GameEngine (animateGame) where

import Prelude

import Control.Monad.Eff (Eff)

import FRP (FRP)
import FRP.Behavior (ABehavior, sample_)
import FRP.Event (Event, subscribe, fold)
import FRP.Event.Time (animationFrame)

animateGame :: forall d a b c . ABehavior Event d  -> (d -> b -> b) -> b -> (b -> Eff (frp :: FRP | c) a) -> Eff (frp :: FRP | c) (Eff (frp :: FRP | c) Unit)
animateGame input gameLoop init renderer = subscribe (fold gameLoop (sample_ input animationFrame) init) renderer 

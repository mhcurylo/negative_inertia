module Main (main) where

import Prelude

import Data.Maybe (fromJust)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Graphics.Drawing (render)
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D)

import Partial.Unsafe (unsafePartial)
import GameEngine (animateGame)
import Game (gameLoop)
import Types (Game(Start, Suspend))
import Draw (drawGameState)
import Input (input)

import FRP (FRP)
foreign import hot :: forall eff. Eff eff Unit

game :: forall e. Eff (frp :: FRP, canvas :: CANVAS | e) Unit
game = do
  mc <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mc)
  ctx <- getContext2D canvas
  _ <- animateGame input gameLoop (Suspend 60 Start) (render ctx <<< drawGameState)
  pure unit

main :: Unit
main = unsafePerformEff $ do
    game
    hot

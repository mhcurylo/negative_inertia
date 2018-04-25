module Main (main) where

import Prelude

import Data.Maybe (fromJust)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Graphics.Drawing (render)
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, getCanvasWidth, getCanvasHeight)

import Partial.Unsafe (unsafePartial)
import GameEngine (animateGame)
import Game (gameLoop)
import Types (Game(Start))
import Draw (drawGameState, drawPongState)
import Input (input, inputPong)
import Pong (initPong, movePong)

import FRP (FRP)
foreign import hot :: forall eff. Eff eff Unit

game :: forall e. Eff (frp :: FRP, canvas :: CANVAS | e) Unit
game = do
  mc <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mc)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  initialPongState <- pure $ initPong w h
  -- _ <- animateGame input gameLoop Start (render ctx <<< drawGameState)
  _ <- animateGame inputPong movePong initialPongState (render ctx <<< drawPongState)
  pure unit

main :: Unit
main = unsafePerformEff $ do
    game
    hot

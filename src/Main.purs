module Main (main) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Graphics.Drawing (scale, translate, shadowBlur, black, shadowColor, rgb,
                         shadow, render, rotate, rectangle, closed, fillColor,
                         white, filled, Drawing)
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D)

import Data.Set as Set
import Data.Int (toNumber)
import Data.Maybe (fromJust, maybe, Maybe)
import Partial.Unsafe (unsafePartial)
import GameLoop (animateGame)

import FRP (FRP)
foreign import hot :: forall eff. Eff eff Unit

data Move = Up | Down | No

type Ball = {
    x :: Number
  , y :: Number
}

drawBall :: Ball -> Drawing
drawBall ({x, y}) = filled (fillColor black) (rectangle x y 5.0 5.0)

circle :: Ball -> Drawing
circle b  = filled (fillColor white) (rectangle 0.0 0.0 1000.0 1000.0) <> drawBall b 

acc :: Set.Set Int -> Ball -> Ball
acc s ({x, y}) = ({x: x + 0.1, y: y + 0.1})

game :: forall e. Eff (frp :: FRP, canvas :: CANVAS | e) Unit
game = do
  mc <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mc)
  ctx <- getContext2D canvas
  _ <- animateGame acc ({x: 2.0, y: 2.0}) (render ctx <<< circle)
  pure unit

main = unsafePerformEff $ do
  game
  hot

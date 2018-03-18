module Game (gameLoop, initialGameState) where

import Prelude (map, ($), join, negate, (+), (-), (<), (<<<), (>), (>=), id, (<$>))
import Types
import Math (abs)
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Array (filter, find, head, mapWithIndex, index, modifyAt)
import Debug.Trace
import Control.Biapply ((<<*>>))
import AABB (Collision, sweepPhysicals)
import Algorithm (foldPairs, listTuple)

accBall :: Ball -> Ball
accBall b = b {vel = vec 4.0 4.8}

initialGameState :: GameState
initialGameState = ({
    ball: accBall (createBall 495.0 300.0)
  , paddles: Tuple (createPaddle 30.0 280.0) (createPaddle 960.0 280.0)
  , scores: Tuple 0 0
  , walls: Tuple (createWall 35.0) (createWall 590.0)
})

accUp :: Vector
accUp = vec 0.0 (-1.0)
accDown :: Vector
accDown = vec 0.0 1.0
accStay :: Vector
accStay = zeroVector  

movePlayer :: Move -> Paddle -> Paddle
movePlayer Up p = p {acc = accUp} 
movePlayer Down p = p {acc =  accDown}
movePlayer Stay p = p {acc = accStay }

playerMoves :: PlayerMoves -> GameState -> GameState
playerMoves pms gs@({paddles}) = gs { paddles = (both movePlayer pms <<*>> paddles) }

movePhysical :: Number -> Physical -> Physical
movePhysical time p@{pos, vel, acc} = p {
    pos = pos + scale time vel
  , vel = vel + scale time acc
  , acc = acc
  }

firstJust :: forall a . Array (Maybe a) -> Maybe a
firstJust = join <<< head <<< filter isJust 

deflect :: Vector -> Vector -> Vector
deflect vel normal = mulV vel (vec nx ny) 
  where
    p x = abs x > 0.0
    nx = if p (getX normal) then (-1.0) else 1.0
    ny = if p (getY normal) then (-1.0) else 1.0

deflectPhysical :: Physical -> Collision -> Physical
deflectPhysical x {time: time, normal: normal} =
  x {
    pos = x.pos + scale time x.vel + scale (1.0 - time) deflectedVel,
    vel = deflect x.vel normal
  }
  where
    deflectedVel = deflect x.vel normal

doPhysical :: Physical -> Array Physical -> Physical
doPhysical thing s =
    case find isJust $ map (sweepPhysicals thing) s of
      Just (Just x) -> deflectPhysical thing x
      otherwise -> movePhysical 1.0 thing

-- | Find a pair of indices of earliest colliding elements in array
-- |
firstCollision :: Array Physical -> Maybe { collision :: Collision, i :: Int, j :: Int }
firstCollision v = foldPairs f Nothing (mapWithIndex Tuple v)
  where
    f z (Tuple i x) (Tuple j y) = case sweepPhysicals x y of
      Just collision -> case z of
        Just zz -> if zz.collision.time < collision.time
                   then z
                   else Just {collision: collision, i: i, j: j}
        Nothing -> Just {collision: collision, i: i, j: j}
      Nothing -> z

deflectPhysical2 :: Collision -> Physical -> Physical
deflectPhysical2 {normal} x = x { vel = deflect x.vel normal }

opposite :: Collision -> Collision
opposite x@{normal} = x {normal = mulV (vec (-1.0) (-1.0)) normal}

simulate :: Number -> Array Physical -> Array Physical
simulate time v =
  case firstCollision v of
    Just {collision, i, j} ->
      if collision.time >= time
      then justMove
      else simulate (time - collision.time) (u2 collision i j (movePhysical collision.time <$> v))
    Nothing -> justMove
  where
    justMove = movePhysical time <$> v
    u c i v = fromMaybe v (modifyAt i (deflectPhysical2 c) v)
    u2 c i j v = u (opposite c) j $ u c i v

move2 :: GameState -> GameState
move2 gs = case ret of
    [b, p1, p2, w1, w2] -> gs {
      ball = b,
      paddles = Tuple p1 p2,
      walls = Tuple w1 w2
    }
    otherwise -> gs
  where
    ret = simulate 1.0 [gs.ball, fst gs.paddles, snd gs.paddles, fst gs.walls, snd gs.walls]

move :: GameState -> GameState
move gs = gs {
    ball = doPhysical gs.ball [fst gs.paddles, snd gs.paddles, fst gs.walls, snd gs.walls],
    paddles = Tuple paddle1 paddle2
  }
  where
    paddle1 = doPhysical (fst gs.paddles) [fst gs.walls, snd gs.walls]
    paddle2 = doPhysical (snd gs.paddles) [fst gs.walls, snd gs.walls]
    _ = spy <$> firstCollision [fst gs.walls, snd gs.walls, fst gs.paddles, snd gs.paddles, gs.ball]

score :: GameState -> GameState
score gs@{ball, scores: (Tuple p1 p2)} = if bx < 0.0
  then initialGameState {scores = Tuple p1 (p2 + 1)}
  else if bx > 1000.0
    then initialGameState {scores = Tuple (p1 + 1) p2}
    else gs
  where
  bx = getX ball.pos      

gameLoop :: PlayerMoves -> GameState -> GameState
gameLoop pm = score <<< move2 <<< playerMoves pm

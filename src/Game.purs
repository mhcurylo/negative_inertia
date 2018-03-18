module Game (gameLoop, initialGameState) where

import Prelude (map, ($), join, negate, (+), (||), (-), (<), (<<<), (>))
import Types
import Math (abs)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Array (filter, find, head)
import Control.Biapply ((<<*>>))
import AABB (Collision, sweepPhysicals)



accBall :: Ball -> Ball
accBall b = b {vel = vec (-4.0) 0.0}

initialGameState :: GameState
initialGameState = ({
    ball: accBall (createBall 495.0 300.0)
  , paddles: Tuple (createPaddle 30.0 280.0) (createPaddle 960.0 280.0)
  , scores: Tuple 0 0
  , walls: Tuple (createWall 35.0) (createWall 590.0)
})


accUp :: Vector
accUp = vec 0.0 (-1.2)
accDown :: Vector
accDown = vec 0.0 1.2
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

-- | Applies collision response
-- | 1. Move physical to the collision point in time
-- | 2. Set physical's velocity to that after collision
-- | 3. Move physical to the point after collision
-- |
deflectPhysical :: Physical -> Collision -> Physical
deflectPhysical x {time: time, normal: normal} =
  movePhysical (1.0 - time)
    ((movePhysical time x) { vel = deflect x.vel normal })

applyInertia :: Physical -> Physical
applyInertia t@({inertia, vel}) = t {
    vel = mulV inertia vel
  }

doPhysical :: Physical -> Array Physical -> Physical
doPhysical thing s =
    case find isJust $ map (sweepPhysicals thing') s of
      Just (Just x) -> deflectPhysical thing' x
      otherwise -> movePhysical 1.0 thing'
    where
    thing' = applyInertia thing      

move :: GameState -> GameState
move gs = gs {
    ball = doPhysical gs.ball [fst gs.paddles, snd gs.paddles, fst gs.walls, snd gs.walls],
    paddles = Tuple paddle1 paddle2
  }
  where
    paddle1 = doPhysical (fst gs.paddles) [fst gs.walls, snd gs.walls]
    paddle2 = doPhysical (snd gs.paddles) [fst gs.walls, snd gs.walls]

score :: GameState -> GameState
score gs@{ball, scores: (Tuple p1 p2)} = if bx < 0.0
  then initialGameState {scores = Tuple p1 (p2 + 1)}
  else if bx > 1000.0
    then initialGameState {scores = Tuple (p1 + 1) p2}
    else gs 
  where
  bx = getX ball.pos      

finish :: GameState -> Game
finish gs@({scores: (Tuple p1 p2)}) = if (p1 > 9) || (p2 > 9)
  then Finish p1 p2
  else Progress gs

resetIfMoved :: PlayerMoves -> Game -> Game
resetIfMoved (Tuple Stay Stay) g = g
resetIfMoved _ g = Progress initialGameState   

gameLoop :: PlayerMoves -> Game -> Game
gameLoop pm (Progress gameState) = finish <<< score <<< move <<< playerMoves pm $ gameState
gameLoop pm g = resetIfMoved pm g 

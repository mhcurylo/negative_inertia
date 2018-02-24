module Game (gameLoop, initialGameState) where

import Prelude ((+), (-), (*), (/), ($), (<), (>), (<<<), (<*>), (<$>), (||), (&&), negate)
import Types 
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Control.Biapply ((<<*>>))

createPhysical inertia w h x y = ({
    pos : {x, y}
  , acc
  , vel
  , size: {x: w, y: h}
  , inertia
}) where
   acc = ({ x: 0.0 , y: 0.0 })
   vel = acc

createPaddle = createPhysical 0.1 10.0 50.0
createBall = createPhysical (-0.001) 5.0 5.0 
createWall = createPhysical 0.0 960.0 5.0 20.0

center :: Physical -> Vector
center ({pos, size}) = ({x: (pos.x + size.x) / 2.0, y: (pos.y + size.y) / 2.0})

ballLine :: Ball -> {a1 :: Vector, a2 :: Vector}
ballLine b@({vel}) = ({
  a1: c,
  a2: {x: c.x - vel.x, y: c.x - vel.y}
})
  where
  c = center b

accBall :: Ball -> Ball
accBall b = b {vel = {x: 0.5, y: 0.5}}

initialGameState :: GameState
initialGameState = ({
    ball: accBall (createBall 495.0 300.0)
  , paddles: Tuple (createPaddle 40.0 50.0) (createPaddle 960.0 520.0)
  , scores: Tuple 0 0
  , walls: Tuple (createWall 35.0) (createWall 590.0)
})

movePlayer :: Move -> Paddle -> Paddle
movePlayer Up p@({pos}) = p {acc = {x: 0.0, y: (- 1.0)}}
movePlayer Down p@({pos}) = p {acc = {x: 0.0, y: 1.0 }}
movePlayer Stay p@({pos}) = p {acc = {x: 0.0, y: 0.0 }}

accelerate :: Physical -> Physical 
accelerate p@{pos, vel, acc, inertia} = p {
    pos = newPos
  , vel = newVel
  , acc = acc
  } where
  newVel = {x: (vel.x + acc.x - vel.x * inertia), y: (vel.y + acc.y - vel.y * inertia)}       
  newPos = {x: (pos.x + vel.x), y: (pos.y + vel.y)}       

playerMoves :: PlayerMoves -> GameState -> GameState
playerMoves pms gs@({paddles}) = gs { paddles = (both movePlayer pms <<*>> paddles) }


move :: GameState -> GameState
move gs@{ball, paddles} = gs {ball = accelerate ball, paddles = both accelerate paddles}


data Eq = Eq Number Number Number

nonZero :: Number -> Maybe Number
nonZero n = if n > 0.0
  then Just 0.0
  else Nothing

denom :: Vector -> Vector -> Vector -> Vector -> Maybe Number
denom a1 a2 b1 b2 = nonZero (((a1.x - a2.x) * (b1.y - b2.y)) - ((a1.y - a2.y) * (b1.x - b2.x)))
fua :: Vector -> Vector -> Vector -> Vector -> Number -> Number
fua a1 a2 b1 b2 d = (((b2.x - b1.x) * (a2.x - a1.x)) - ((b2.x - b1.x) * (a2.y - a1.y))) / d
fub :: Vector -> Vector -> Vector -> Vector -> Number -> Number
fub a1 a2 b1 b2 d = (((a2.x - a1.x) * (a1.y - b1.y)) - ((a2.y - a1.y) * (a1.x - b1.x))) / d
intersected :: Number -> Number -> Boolean
intersected ua ub = (ua < 0.0) || (ua > 1.0) || (ub < 0.0) || (ub > 1.0) 

intersectLines :: Vector -> Vector -> Vector -> Vector -> Boolean
intersectLines a1 a2 b1 b2 = fromMaybe false $ intersected <$> (fua a1 a2 b1 b2 <$> d) <*> (fub a1 a2 b1 b2 <$> d)
  where
  d = denom a1 a2 b1 b2

collideWithWalls :: GameState -> GameState
collideWithWalls gs@{ball, paddles, walls: Tuple w1 w2} = gs { 
      ball = collideWalls ball
    , paddles = both collideWalls paddles 
  } where
  collideWalls = collideTopWall <<< collideBottomWall 
  insideWall p w = p.pos.x + p.size.x > w.pos.x && p.pos.x < w.pos.x + w.size.x
  collideTopWall p@({pos, vel}) = if insideWall p w1 && pos.y < w1.pos.y + w1.size.y
                             then p {
                                 vel = {x: vel.x, y: -vel.y}
                               , pos = {x: pos.x, y: w1.pos.y + w1.size.y}
                             }
                             else p
  collideBottomWall p@({pos, vel, size}) = if insideWall p w2 && pos.y + size.y > w2.pos.y 
                             then p {
                                 vel = {x: vel.x, y: -vel.y}
                               , pos = {x: pos.x, y: w2.pos.y - size.y}
                             }
                             else p

pongTheBall :: GameState -> GameState
pongTheBall gs@{ball, paddles: Tuple p1 p2} = gs {
  ball = ball 
}
  where 
  ({a1, a2}) = ballLine ball      
  pongP1 = Nothing
  pongP2 = Nothing   

score x = x     

gameLoop :: PlayerMoves -> GameState -> GameState
gameLoop pm = score <<< pongTheBall <<< collideWithWalls <<< move <<< playerMoves pm

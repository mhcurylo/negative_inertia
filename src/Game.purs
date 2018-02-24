module Game (gameLoop, initialGameState) where

import Prelude ((+), (-), (*), (/), ($), (<), (>), (<<<), (<*>), (<$>), (==), (||), (&&), negate, not)
import Types 
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Control.Biapply ((<<*>>))
import Control.Alt ((<|>))
-- import Debug.Trace

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

type Line = {v1 :: Vector, v2 :: Vector}


ballLine :: Ball -> Line 
ballLine b@({vel}) = ({
  v1: c,
  v2: {x: c.x - vel.x, y: c.x - vel.y}
})
  where
  c = center b

paddleLeftLine :: Paddle -> Line
paddleLeftLine ({size, pos: p@{x, y}, vel}) 
  | vel.y > 0.0 = ({
       v1: {x: x + size.x, y}
     , v2: {x: x + vel.x, y}
  })
  | true = ({
       v1: p
     , v2: {x: x + size.x + vel.x, y}
  })

paddleRightLine :: Paddle -> Line
paddleRightLine p@({size}) = ({
      v1: rightY v1 
    , v2: rightY v2
})
  where
  rightY ({x, y}) = ({x, y: y + size.y}) 
  ({v1, v2}) = paddleLeftLine p

accBall :: Ball -> Ball
accBall b = b {vel = {x: 2.0, y: 0.0}}

initialGameState :: GameState
initialGameState = ({
    ball: accBall (createBall 495.0 300.0)
  , paddles: Tuple (createPaddle 30.0 280.0) (createPaddle 960.0 280.0)
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

filterMaybe :: forall a . (a -> Boolean)  -> Maybe a -> Maybe a
filterMaybe f j@(Just a) = if f a then j else Nothing
filterMaybe f Nothing = Nothing  

nonZero :: Number -> Maybe Number
nonZero = filterMaybe (\v -> v > 0.0) <<< Just

denom :: Vector -> Vector -> Vector -> Vector -> Maybe Number
denom a1 a2 b1 b2 = nonZero (((a1.x - a2.x) * (b1.y - b2.y)) - ((a1.y - a2.y) * (b1.x - b2.x)))
fua :: Vector -> Vector -> Vector -> Vector -> Number -> Number
fua a1 a2 b1 b2 d = (((b2.x - b1.x) * (a2.x - a1.x)) - ((b2.x - b1.x) * (a2.y - a1.y))) / d
fub :: Vector -> Vector -> Vector -> Vector -> Number -> Number
fub a1 a2 b1 b2 d = (((a2.x - a1.x) * (a1.y - b1.y)) - ((a2.y - a1.y) * (a1.x - b1.x))) / d
intersected :: Number -> Number -> Boolean
intersected ua ub = (ua < 0.0) || (ua > 1.0) || (ub < 0.0) || (ub > 1.0) 

intersectLines :: Line -> Line -> Maybe Boolean
intersectLines a b = filterMaybe (\m -> m == false) $ not <$> intersected <$> (fua a1 a2 b1 b2 <$> d) <*> (fub a1 a2 b1 b2 <$> d)
  where
  a1 = a.v1
  a2 = a.v2
  b1 = b.v1
  b2 = b.v2
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
  ball = fromMaybe ball (pongP1 <|> pongP2)
}
  where 
  l = ballLine ball      
  pongP1 = (\_ -> ball { vel = {x: ball.vel.x * -1.0, y: ball.vel.y}, pos = ball.pos } ) <$> intersectLines l (paddleRightLine p1)
  pongP2 = (\_ -> ball { vel = {x: ball.vel.x * -1.0, y: ball.vel.y}, pos = ball.pos } ) <$> intersectLines l (paddleLeftLine p2) 

score x = x     

gameLoop :: PlayerMoves -> GameState -> GameState
gameLoop pm = score <<< pongTheBall <<< collideWithWalls <<< move <<< playerMoves pm

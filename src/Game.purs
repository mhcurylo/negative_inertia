module Game (gameLoop, initialGameState) where

import AABB (Collision)
import Control.Biapply ((<<*>>))
import Data.Array (filter, head)
import Data.Maybe (Maybe, isJust)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Physics (simulate, defaultCollide, composeCollisions, CollisionHandler)
import Prelude (join, negate, ($), (*), (+), (-), (<), (<$>), (<<<), (>), (||))
import Types (Ball, Game(..), GameState, Move(..), Paddle, Physical, PhysicalKind(..), PlayerMoves, both, createBall, createPaddle, createWall)
import Vector (Vector, getX, scale, vec, origin)

startingBall :: Boolean -> Ball
startingBall dirR = (createBall 495.0 300.0) {vel = (vec 4.0 0.0) * accMod}
  where
  accMod = if dirR then vec (1.0) (1.0) 
             else vec (-1.0) (-1.0)

initialGameState :: GameState
initialGameState = ({
    ball: startingBall true
  , paddles: Tuple (createPaddle 30.0 280.0) (createPaddle 960.0 280.0)
  , scores: Tuple 0 0
  , walls: Tuple (createWall 35.0) (createWall 590.0)
})

initGameState :: Boolean -> Int -> Int -> GameState
initGameState dirR p1s p2s = initialGameState {
      scores = Tuple p1s p2s
    , ball = startingBall dirR
  }

accUp :: Vector
accUp = vec 0.0 (-1.5)
accDown :: Vector
accDown = vec 0.0 1.5
accStay :: Vector
accStay = origin  

movePlayer :: Move -> Paddle -> Paddle
movePlayer Up p = p {acc = accUp} 
movePlayer Down p = p {acc =  accDown}
movePlayer Stay p = p {acc = accStay }

playerMoves :: PlayerMoves -> GameState -> GameState
playerMoves pms gs@({paddles}) = gs { paddles = (both movePlayer pms <<*>> paddles) }

firstJust :: forall a . Array (Maybe a) -> Maybe a
firstJust = join <<< head <<< filter isJust 

applyInertia :: Physical -> Physical
applyInertia t@({inertia, vel}) = t {
    vel = inertia * vel
  }

responsiveBall :: Collision -> Ball -> Paddle -> Ball
responsiveBall collision ball@{vel} paddle = ball { vel = vel + scale 0.2 paddle.vel }

ballVsPaddle :: CollisionHandler
ballVsPaddle c x@{kind: Ball} y@{kind: Paddle} = Tuple (responsiveBall c x y) y
ballVsPaddle c x@{kind: Paddle} y@{kind: Ball} = Tuple x (responsiveBall c y x)
ballVsPaddle c x y = Tuple x y

collide :: CollisionHandler
collide = composeCollisions ballVsPaddle defaultCollide

move :: GameState -> GameState
move gs = case ret of
    [p1, p2, w1, w2, b] -> gs {
      ball = b,
      paddles = Tuple p1 p2,
      walls = Tuple w1 w2
    }
    otherwise -> gs
  where
    ret = applyInertia <$> simulate collide 1.0 [fst gs.paddles, snd gs.paddles, fst gs.walls, snd gs.walls, gs.ball]

score :: GameState -> GameState
score gs@{ball, scores: (Tuple p1 p2)} = if bx < (-50.0)
  then initGameState false p1 (p2 + 1)
  else if bx > 1050.0
    then initGameState true (p1 + 1) p2
    else gs 
  where
  bx = getX ball.pos      

finish :: GameState -> Game
finish gs@({scores: (Tuple p1 p2)}) = if (p1 > 9) || (p2 > 9)
  then Suspend 60 $ Finish p1 p2
  else Progress gs

resetIfMoved :: PlayerMoves -> Game -> Game
resetIfMoved (Tuple Stay Stay) g = g
resetIfMoved _ g = Progress initialGameState   

gameLoop :: PlayerMoves -> Game -> Game
gameLoop pm (Progress gameState) = finish <<< score <<< move <<< playerMoves pm $ gameState
gameLoop pm  (Suspend 0 g) = gameLoop pm g
gameLoop _  (Suspend i g) = Suspend (i - 1) (gameLoop (Tuple Stay Stay) g)
gameLoop pm g = resetIfMoved pm g 

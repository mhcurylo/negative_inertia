module Game (gameLoop, initialGameState) where

import Prelude (join, negate, (+), (<), (<<<), (>), (<$>), (||), ($))
import Types
import Data.Maybe (Maybe, isJust)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Array (filter, head)
import Control.Biapply ((<<*>>))
import Physics (simulate, defaultCollide)



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

firstJust :: forall a . Array (Maybe a) -> Maybe a
firstJust = join <<< head <<< filter isJust 

applyInertia :: Physical -> Physical
applyInertia t@({inertia, vel}) = t {
    vel = mulV inertia vel
  }

move :: GameState -> GameState
move gs = case ret of
    [b, p1, p2, w1, w2] -> gs {
      ball = b,
      paddles = Tuple p1 p2,
      walls = Tuple w1 w2
    }
    otherwise -> gs
  where
    ret = applyInertia <$> simulate defaultCollide 1.0 [gs.ball, fst gs.paddles, snd gs.paddles, fst gs.walls, snd gs.walls]

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

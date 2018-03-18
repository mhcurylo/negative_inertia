module Draw (drawGameState) where

import Prelude (join, show, ($), (<>), (>))
import Graphics.Drawing.Font (font, sansSerif, bold, Font)
import Graphics.Drawing (Drawing, black, fillColor, filled, rectangle, text, white)
import Data.Tuple (Tuple(Tuple))
import Data.Foldable (foldMap)

import Types 

showScores :: Int -> Int -> String
showScores p1 p2 = show p1 <> " : " <> show p2

scoreFont :: Font
scoreFont = font sansSerif 18 bold 

drawSquare :: Physical -> Drawing
drawSquare ({pos, size}) = filled (fillColor white) (rectangle (getX pos) (getY pos) (getX size) (getY size)) 

drawScores :: Scores -> Drawing
drawScores (Tuple p1 p2) = text scoreFont 490.0 25.0 (fillColor white) (showScores p1 p2)

listTuple :: forall a . Tuple a a -> Array a
listTuple (Tuple x y) = [x, y]

background :: Drawing
background = filled (fillColor black) (rectangle 0.0 0.0 1000.0 6000.0) 

mainText :: String -> Drawing 
mainText = text scoreFont 100.0 100.0 (fillColor white)

finalScores :: String -> Drawing 
finalScores = text scoreFont 100.0 200.0 (fillColor white)

pressToPlay :: Drawing 
pressToPlay = text scoreFont 100.0 300.0 (fillColor white) "Press to play!"

drawGameState :: Game -> Drawing
drawGameState (Progress ({ball, paddles, scores, walls}))  = background
  <>  (foldMap drawSquare $ join [listTuple paddles, listTuple walls, [ball]])
  <> drawScores scores
drawGameState (Start) = background <> pressToPlay <> mainText "NEGATIVE INERTIA"
drawGameState (Finish p1 p2) = background <> pressToPlay <> mainText (winner <> " WON") <> finalScores (showScores p1 p2)
  where
  winner = if p1 > p2 then "P1" else "P2"

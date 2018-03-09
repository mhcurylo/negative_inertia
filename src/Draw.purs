module Draw (drawGameState) where

import Prelude (join, show, ($), (<>))
import Graphics.Drawing.Font (font, sansSerif, bold, Font)
import Graphics.Drawing (Drawing, black, fillColor, filled, rectangle, text, white)
import Data.Tuple (Tuple(Tuple))
import Data.Foldable (foldMap)

import Types 

scoreFont :: Font
scoreFont = font sansSerif 18 bold 

drawSquare :: Physical -> Drawing
drawSquare ({pos, size}) = filled (fillColor white) (rectangle (getX pos) (getY pos) (getX size) (getY size)) 

drawScores :: Scores -> Drawing
drawScores (Tuple p1 p2) = text scoreFont 490.0 25.0 (fillColor white) (show p1 <> " : " <> show p2) 

listTuple :: forall a . Tuple a a -> Array a
listTuple (Tuple x y) = [x, y]

drawGameState :: GameState -> Drawing
drawGameState ({ball, paddles, scores, walls})  = filled (fillColor black) (rectangle 0.0 0.0 1000.0 6000.0) 
  <>  (foldMap drawSquare $ join [listTuple paddles, listTuple walls, [ball]])
  <> drawScores scores

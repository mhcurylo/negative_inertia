module Input (input) where

import Prelude (($), (<$>), (<<<), flip)
import Data.Tuple (Tuple(Tuple))
import Data.Bifunctor (bimap)
import Data.Map (Map, fromFoldable, filterKeys, values)
import Data.Set (Set, member)
import Data.List (head)
import Data.Maybe (fromMaybe) 
import FRP.Behavior.Keyboard (keys)
import FRP.Behavior (ABehavior)
import FRP.Event (Event)
import Types (Move(Down, Up, Stay), PlayerMoves)
import Pong (PongInput)
 
type KeyMap = Map Int Move
type KeyMaps = Tuple KeyMap KeyMap 

playerKeys :: Int -> Int -> KeyMap 
playerKeys up down = fromFoldable [Tuple up Up, Tuple down Down]

keyMaps :: KeyMaps 
keyMaps = Tuple (playerKeys 65 90) (playerKeys 38 40)  

mapToMove :: Set Int -> KeyMap -> Move
mapToMove pressedKeys = fromMaybe Stay <<< head <<< values <<< filterKeys (flip member $ pressedKeys)

keysToMove :: Set Int -> PlayerMoves
keysToMove pressedKeys = bimap toMove toMove keyMaps 
  where
  toMove = mapToMove pressedKeys

keysToGameInput :: Set Int -> PongInput
keysToGameInput pressedKeys = {
    player1: {
      up: member 65 pressedKeys,
      down: member 90 pressedKeys
    },
    player2: {
      up: member 38 pressedKeys,
      down: member 40 pressedKeys
    }
  }

input :: ABehavior Event PlayerMoves
input = keysToMove <$> keys  





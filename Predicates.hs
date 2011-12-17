module Predicates where

import Types

import Data.Label
import qualified Data.Set as Set


-- Objects

hasColor :: Color -> Object -> Bool
hasColor c o = c `Set.member` get colors o

isOwnedBy :: Ref Player -> Object -> Bool
isOwnedBy rp o = rp == get owner o

isControlledBy :: Ref Player -> Object -> Bool
isControlledBy rp o = rp == get controller o


-- Zones

isInLibrary :: Object -> Bool
isInLibrary o =
  case get zone o of
    Library -> True
    _ -> False

isInHand :: Object -> Bool
isInHand o =
  case get zone o of
    Hand -> True
    _ -> False

isOnStack :: Object -> Bool
isOnStack o =
  case get zone o of
    Stack _ -> True
    _ -> False

isOnBattlefield :: Object -> Bool
isOnBattlefield o =
  case get zone o of
    Battlefield _ -> True
    _ -> False

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

isInZone :: Zone -> Object -> Bool
isInZone z o = get zone o == z

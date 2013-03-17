module Magic.Predicates (hasColor, isOwnedBy, isControlledBy) where

import Magic.Types

import Data.Label
import qualified Data.Set as Set


-- Objects

hasColor :: Color -> Object -> Bool
hasColor c o = c `Set.member` get colors o

isOwnedBy :: PlayerRef -> Object -> Bool
isOwnedBy pr o = pr == get owner o

isControlledBy :: PlayerRef -> Object -> Bool
isControlledBy pr o = pr == get controller o

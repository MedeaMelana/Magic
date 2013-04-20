module Magic.Predicates (
    hasColor, isOwnedBy, isControlledBy, hasTypes, hasPermanentType
  ) where

import Magic.Types
import Magic.ObjectTypes

import Data.Boolean
import Data.Label
import qualified Data.Set as Set


-- Objects

hasColor :: Color -> Object -> Bool
hasColor c o = c `Set.member` get colors o

isOwnedBy :: PlayerRef -> Object -> Bool
isOwnedBy pr o = pr == get owner o

isControlledBy :: PlayerRef -> Object -> Bool
isControlledBy pr o = pr == get controller o

-- | Checks whether the object's types are a superset of the given type set.
hasTypes :: ObjectTypes -> Object -> Bool
hasTypes t o = t `isObjectTypesSubsetOf` _types o

-- | Checks whether the object has at least one of the permanent card types.
hasPermanentType :: Object -> Bool
hasPermanentType = gor $ map hasTypes [artifactType, creatureType, enchantmentType, landType, planeswalkerType]

gor :: Boolean b => [b] -> b
gor = foldr (||*) false

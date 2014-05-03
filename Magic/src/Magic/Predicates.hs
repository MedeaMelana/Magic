{-# LANGUAGE DataKinds #-}

module Magic.Predicates (
    hasColor, isOwnedBy, isControlledBy, hasTypes, hasPermanentType, checkPermanent
  ) where

import Magic.Core (object, objectPart)
import Magic.Labels ((.^))
import Magic.ObjectTypes
import Magic.Types
import Magic.Utils (gor)

import Control.Applicative ((<$>))
import Data.Label (get)
import Data.Label.Monadic (asks)
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

checkPermanent :: (Object -> Bool) -> ObjectRef TyPermanent -> View Bool
checkPermanent ok r = ok <$> asks (object r .^ objectPart)

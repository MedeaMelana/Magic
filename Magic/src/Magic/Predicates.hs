{-# LANGUAGE DataKinds #-}

module Magic.Predicates (
    hasColor,
    isOwnedBy,
    isControlledBy,
    hasName,
    hasTypes,
    hasOneOfTypes,
    hasPermanentType,
    checkPermanent,
    hasStaticKeywordAbility
  ) where

import Magic.Core (object, objectPart)
import Magic.ObjectTypes
import Magic.Types
import Magic.Utils (gor)

import Control.Applicative ((<$>))
import Control.Category ((.))
import Data.Label (get)
import Data.Label.Monadic (asks)
import qualified Data.Set as Set
import Data.Text (Text)
import Prelude hiding ((.))


-- Objects

hasName :: Text -> Object -> Bool
hasName t o = pure t == get name o

hasColor :: Color -> Object -> Bool
hasColor c o = c `Set.member` get colors o

isOwnedBy :: PlayerRef -> Object -> Bool
isOwnedBy pr o = pr == get owner o

isControlledBy :: PlayerRef -> Object -> Bool
isControlledBy pr o = pr == get controller o

-- | Checks whether the object's types are a superset of the given type set.
hasTypes :: ObjectTypes -> Object -> Bool
hasTypes t o = t `isObjectTypesSubsetOf` _types o

-- | Checks whether the object has one of the given type set.
hasOneOfTypes :: [ObjectTypes] -> Object -> Bool
hasOneOfTypes ts o = any (`hasTypes` o) ts

-- | Checks whether the object has at least one of the permanent card types.
hasPermanentType :: Object -> Bool
hasPermanentType = gor $ map hasTypes [artifactType, creatureType, enchantmentType, landType, planeswalkerType]

-- | Checks whether the object has a given static keyword ability.
hasStaticKeywordAbility:: StaticKeywordAbility -> Object -> Bool
hasStaticKeywordAbility a o = a `elem` get staticKeywordAbilities o


checkPermanent :: (Object -> Bool) -> ObjectRef TyPermanent -> View Bool
checkPermanent ok r = ok <$> asks (objectPart . object r)

{-# LANGUAGE TypeOperators #-}

module ObjectTypes (
    -- * Convenient type sets
    basicType, legendaryType,
    artifactType, creatureType, enchantmentType, instantType, landType,
    planeswalkerType, sorceryType,

    -- * Class @ObjectType@
    ObjectType(..), objectType, hasTypes,
  ) where

import Types

import Data.Label.Pure (set, (:->))
import Data.Monoid (mempty)
import Data.Set (Set)
import qualified Data.Set as Set


basicType :: ObjectTypes
basicType = mempty { _supertypes = Set.singleton Basic }

legendaryType :: ObjectTypes
legendaryType = mempty { _supertypes = Set.singleton Legendary }

artifactType :: ObjectTypes
artifactType = mempty { _artifactSubtypes = Just mempty }

creatureType :: ObjectTypes
creatureType = mempty { _creatureSubtypes = Just mempty }

enchantmentType :: ObjectTypes
enchantmentType = mempty { _enchantmentSubtypes = Just mempty }

instantType :: ObjectTypes
instantType = mempty { _instantSubtypes = Just mempty }

landType :: ObjectTypes
landType = mempty { _landSubtypes = Just mempty }

planeswalkerType :: ObjectTypes
planeswalkerType = mempty { _planeswalkerSubtypes = Just mempty }

sorceryType :: ObjectTypes
sorceryType = mempty { _sorcerySubtypes = Just mempty }


class ObjectType a where
  objectTypeLabel :: ObjectTypes :-> Maybe (Set a)

instance ObjectType ArtifactSubtype where
  objectTypeLabel = artifactSubtypes

instance ObjectType CreatureSubtype where
  objectTypeLabel = creatureSubtypes

instance ObjectType EnchantmentSubtype where
  objectTypeLabel = enchantmentSubtypes

instance ObjectType LandSubtype where
  objectTypeLabel = landSubtypes

instance ObjectType PlaneswalkerSubtype where
  objectTypeLabel = planeswalkerSubtypes

objectType :: ObjectType a => a -> ObjectTypes
objectType ty = set objectTypeLabel (Just (Set.singleton ty)) mempty

hasTypes :: Object -> ObjectTypes -> Bool
hasTypes o t = t `isObjectTypesSubsetOf` _types o

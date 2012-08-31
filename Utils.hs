{-# LANGUAGE TypeOperators #-}

module Utils where

import Types

import Data.Label.Pure
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set


basicType :: ObjectTypes
basicType = mempty { _supertypes = Set.singleton Basic }

legendaryType :: ObjectTypes
legendaryType = mempty { _supertypes = Set.singleton Legendary }

artifactType :: ObjectTypes
artifactType = mempty { _artifactTypes = Just mempty }

enchantmentType :: ObjectTypes
enchantmentType = mempty { _enchantmentTypes = Just mempty }

instantType :: ObjectTypes
instantType = mempty { _instantTypes = Just mempty }

landType :: ObjectTypes
landType = mempty { _landTypes = Just mempty }

sorceryType :: ObjectTypes
sorceryType = mempty { _sorceryTypes = Just mempty }


class ObjectType a where
  objectTypeLabel :: ObjectTypes :-> Maybe (Set a)

instance ObjectType ArtifactType where
  objectTypeLabel = artifactTypes

instance ObjectType CreatureType where
  objectTypeLabel = creatureTypes

instance ObjectType EnchantmentType where
  objectTypeLabel = enchantmentTypes

instance ObjectType LandType where
  objectTypeLabel = landTypes

instance ObjectType PlaneswalkerType where
  objectTypeLabel = planeswalkerTypes

objectType :: ObjectType a => a -> ObjectTypes
objectType ty = set objectTypeLabel (Just (Set.singleton ty)) mempty

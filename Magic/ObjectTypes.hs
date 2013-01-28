{-# LANGUAGE TypeOperators #-}

module Magic.ObjectTypes (
    -- * Types
    ObjectTypes(..), Supertype(..), ArtifactSubtype(..), CreatureSubtype(..),
    EnchantmentSubtype(..), SpellSubtype(..), LandSubtype(..),
    PlaneswalkerSubtype(..),

    -- * Convenient type sets
    -- | These type sets all contain a single value. This, in combination with
    -- the 'ObjectTypes' 'Monoid' instance, allows for convenient type set construction.
    -- For example, to construct the type line @Basic Land@, use
    -- @'basicType' \<\> 'landType'@.
    basicType, legendaryType,
    artifactType, creatureType, enchantmentType, instantType, landType,
    planeswalkerType, sorceryType,

    -- * Class @ObjectType@
    ObjectType(..), objectTypes, objectType,

    -- * Testing
    isObjectTypesSubsetOf,  hasTypes
  ) where

import Magic.Types

import Data.Label.Pure (set, (:->))
import Data.Monoid (mempty)
import Data.Set (Set)
import qualified Data.Set as Set



-- CONVENIENT TYPE SETS


-- | Supertype 'Basic'.
basicType :: ObjectTypes
basicType = mempty { _supertypes = Set.singleton Basic }

-- | Supertype 'Legendary'.
legendaryType :: ObjectTypes
legendaryType = mempty { _supertypes = Set.singleton Legendary }

-- | Card type @Artifact@.
artifactType :: ObjectTypes
artifactType = mempty { _artifactSubtypes = Just mempty }

-- | Card type @Creature@.
creatureType :: ObjectTypes
creatureType = mempty { _creatureSubtypes = Just mempty }

-- | Card type @Enchantment@.
enchantmentType :: ObjectTypes
enchantmentType = mempty { _enchantmentSubtypes = Just mempty }

-- | Card type @Instant@.
instantType :: ObjectTypes
instantType = mempty { _instantSubtypes = Just mempty }

-- | Card type @Land@.
landType :: ObjectTypes
landType = mempty { _landSubtypes = Just mempty }

-- | Card type @Planeswalker@.
planeswalkerType :: ObjectTypes
planeswalkerType = mempty { _planeswalkerSubtypes = Just mempty }

-- | Card type @Sorcery@.
sorceryType :: ObjectTypes
sorceryType = mempty { _sorcerySubtypes = Just mempty }



-- CLASS OBJECTTYPE


-- | Convenient access to 'ObjectTypes' labels for various subtypes.
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

-- | Allows for convenient construction of type lines with
-- multiple subtypes of one card type. For example, to create Snapcaster Mage's
-- typeline, use @objectTypes ['Human', 'Wizard']@.
objectTypes :: Ord a => ObjectType a => [a] -> ObjectTypes
objectTypes tys = set objectTypeLabel (Just (Set.fromList tys)) mempty

-- | Allows for convenient construction of type lines with
-- subtypes of various card types. For example, to create Dryad Arbor's
-- typeline, use @objectType 'Forest' \<\> objectType 'Dryad'@.
objectType :: ObjectType a => a -> ObjectTypes
objectType ty = set objectTypeLabel (Just (Set.singleton ty)) mempty



-- TESTING


-- | @x \`isObjectTypesSubsetOf\` y@ returns whether all types in @x@ are also in @y@.
isObjectTypesSubsetOf :: ObjectTypes -> ObjectTypes -> Bool
isObjectTypesSubsetOf x y =
    _supertypes x           `Set.isSubsetOf`  _supertypes y &&
    _artifactSubtypes x     `isMaybeSubsetOf` _artifactSubtypes y &&
    _creatureSubtypes x     `isMaybeSubsetOf` _creatureSubtypes y &&
    _enchantmentSubtypes x  `isMaybeSubsetOf` _enchantmentSubtypes y &&
    _instantSubtypes x      `isMaybeSubsetOf` _instantSubtypes y &&
    _landSubtypes x         `isMaybeSubsetOf` _landSubtypes y &&
    _planeswalkerSubtypes x `isMaybeSubsetOf` _planeswalkerSubtypes y &&
    _sorcerySubtypes x      `isMaybeSubsetOf` _sorcerySubtypes y
  where
    isMaybeSubsetOf :: Ord a => Maybe (Set a) -> Maybe (Set a) -> Bool
    Nothing `isMaybeSubsetOf` _ = True
    Just _  `isMaybeSubsetOf` Nothing = False
    Just x'  `isMaybeSubsetOf` Just y' = x' `Set.isSubsetOf` y'

-- | Checks whether the object's types are a superset of the given type set.
hasTypes :: ObjectTypes -> Object -> Bool
hasTypes t o = t `isObjectTypesSubsetOf` _types o

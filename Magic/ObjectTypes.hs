{-# LANGUAGE TypeOperators #-}

module Magic.ObjectTypes (
    -- * Types
    ObjectTypes(..), Supertype(..), ArtifactSubtype(..), CreatureSubtype(..),
    EnchantmentSubtype(..), SpellSubtype(..), LandSubtype(..),
    PlaneswalkerSubtype(..),

    -- * Convenient type sets
    -- | These helper values, in combination with the 'ObjectTypes' 'Monoid' instance,
    -- allow for convenient type set construction. For example, to construct the type
    -- line @Basic Land@, use @'basicType' \<\> 'landType'@.
    basicType, legendaryType,
    artifactType, equipmentType,
    creatureType, creatureTypes,
    enchantmentType, auraType, curseType,
    instantType, instantTypes,
    landType, landTypes,
    planeswalkerType, planeswalkerWithType,
    sorceryType, sorceryTypes,

    -- * Testing
    isObjectTypesSubsetOf, hasTypes, hasPermanentType
  ) where

import Magic.Types

import Data.Boolean (Boolean(..))
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

-- | Card type @Artifact - 'Equipment'@.
equipmentType :: ObjectTypes
equipmentType = mempty { _artifactSubtypes = Just (Set.singleton Equipment) }

-- | Card type @Creature@.
creatureType :: ObjectTypes
creatureType = creatureTypes []

-- | Card type @Creature@ with the specified creature types.
creatureTypes :: [CreatureSubtype] -> ObjectTypes
creatureTypes tys = mempty { _creatureSubtypes = Just (Set.fromList tys) }

-- | Card type @Enchantment@.
enchantmentType :: ObjectTypes
enchantmentType = mempty { _enchantmentSubtypes = Just mempty }

-- | Card type @Enchantment - 'Aura'@.
auraType :: ObjectTypes
auraType = mempty { _enchantmentSubtypes = Just (Set.singleton Aura) }

-- | Card type @Enchantment - 'Curse'@.
curseType :: ObjectTypes
curseType = mempty { _enchantmentSubtypes = Just (Set.singleton Curse) }

-- | Card type @Instant@.
instantType :: ObjectTypes
instantType = instantTypes []

-- | Card type @Instant@ with the specified spell types.
instantTypes :: [SpellSubtype] -> ObjectTypes
instantTypes tys = mempty { _instantSubtypes = Just (Set.fromList tys) }

-- | Card type @Land@.
landType :: ObjectTypes
landType = landTypes []

-- | Card type @Land@ with the specified land types.
landTypes :: [LandSubtype] -> ObjectTypes
landTypes tys = mempty { _landSubtypes = Just (Set.fromList tys) }

-- | Card type @Planeswalker@.
planeswalkerType :: ObjectTypes
planeswalkerType = mempty { _planeswalkerSubtypes = mempty }

-- | Card type @Planeswalker@ with the specified planeswalker type.
planeswalkerWithType :: [PlaneswalkerSubtype] -> ObjectTypes
planeswalkerWithType tys = mempty { _planeswalkerSubtypes = Just (Set.fromList tys) }

-- | Card type @Sorcery@.
sorceryType :: ObjectTypes
sorceryType = mempty { _sorcerySubtypes = Just mempty }

-- | Card type @Sorcery@ with the specified spell types.
sorceryTypes :: [SpellSubtype] -> ObjectTypes
sorceryTypes tys = mempty { _sorcerySubtypes = Just (Set.fromList tys) }



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

-- | Checks whether the object has at least one of the permanent card types.
hasPermanentType :: Object -> Bool
hasPermanentType = gor $ map hasTypes [artifactType, creatureType, enchantmentType, landType, planeswalkerType]

gor :: Boolean b => [b] -> b
gor = foldr (||*) false
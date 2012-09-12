{-# LANGUAGE TypeOperators #-}

module Utils where

import qualified IdList
import Types

import Control.Monad.State (State, execState)
import Data.Label.Pure
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set


mkCard :: State Object () -> Card
mkCard f = Card (\ts rOwner -> execState f (object ts rOwner))

player :: Player
player = Player
  { _life            = 20
  , _manaPool        = []
  , _prestack        = []
  , _library         = IdList.empty
  , _hand            = IdList.empty
  , _graveyard       = IdList.empty
  , _maximumHandSize = Just 7
  , _failedCardDraw  = False
  }

object :: Timestamp -> PlayerRef -> Object
object ts rOwner = Object
  { _name = Nothing
  , _colors = mempty
  , _types = mempty
  , _owner = rOwner
  , _controller = rOwner
  , _timestamp = ts
  , _counters = mempty

  , _tapStatus = Nothing

  , _power = Nothing
  , _toughness = Nothing
  , _damage = Nothing

  , _play = Nothing
  , _staticKeywordAbilities = []
  , _continuousEffects = []
  , _activatedAbilities = []
  , _triggeredAbilities = []
  , _replacementEffects = []
  }


basicType :: ObjectTypes
basicType = mempty { _supertypes = Set.singleton Basic }

legendaryType :: ObjectTypes
legendaryType = mempty { _supertypes = Set.singleton Legendary }

artifactType :: ObjectTypes
artifactType = mempty { _artifactSubtypes = Just mempty }

enchantmentType :: ObjectTypes
enchantmentType = mempty { _enchantmentSubtypes = Just mempty }

instantType :: ObjectTypes
instantType = mempty { _instantSubtypes = Just mempty }

landType :: ObjectTypes
landType = mempty { _landSubtypes = Just mempty }

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

{-# LANGUAGE OverloadedStrings #-}

module BasicLands where

import Types
import Predicates

import Data.Boolean
import Data.Monoid
import Data.Set as Set
import Data.Label.PureM


plains :: Card
plains = Card $ \ts rOwner ->
  Object
  { _name = Just "Plains"
  , _colors = mempty
  , _group = Permanent
    { _supertypes         = Set.singleton Basic
    , _artifactTypes      = Nothing
    , _creatureTypes      = Nothing
    , _enchantmentTypes   = Nothing
    , _landTypes          = Just (Set.singleton Plains)
    , _planeswalkerTypes  = Nothing
    }
  , _zone = Library
  , _owner = rOwner
  , _controller = rOwner
  , _timestamp = ts
  , _counters = []

  , _tapStatus = Nothing

  , _power = Nothing
  , _toughness = Nothing
  , _damage = Nothing

  , _play = defaultPlay
  , _staticKeywordAbilities = []
  , _continuousEffects = []
  , _activatedAbilities = [tapToAddMana (Just White)]
  , _triggeredAbilities = []
  , _replacementEffects = []
  }

defaultPlay :: Ability
defaultPlay rSource rActivator = ClosedAbility
  { _available = do
      os <- asks objects
      let source = os ! rSource
      let ok = (isControlledBy rActivator &&* isInZone Hand) source
      return ok
  , _manaCost = mempty
  , _additionalCosts = []
  , _effect = SpecialAction (return [MoveObject rSource Library Battlefield])
  }

tapToAddMana :: Maybe Color -> Ability
tapToAddMana mc rSource rActivator = ClosedAbility
  { _available = do
      os <- asks objects
      let source = os ! rSource
      let ok = (isControlledBy rActivator &&* isInZone Battlefield) source
      return ok
  , _manaCost = mempty
  , _additionalCosts = []
  , _effect = SpecialAction (return [AddToManaPool rActivator mc])
  }

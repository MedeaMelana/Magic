{-# LANGUAGE OverloadedStrings #-}

module BasicLands where

import Types
import Predicates

import Data.Boolean
import Data.Monoid
import Data.Set as Set
import Data.Label.PureM


plains :: Card
plains = Card $ \ts rOwner rSelf ->
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

  , _play = Ability
    { _available = \rp -> do
        os <- asks objects
        let self = os ! rSelf
        let ok = (isControlledBy rp &&* isInZone Hand) self
        return ok
    , _manaCost = mempty
    , _additionalCosts = []
    , _effect = SpecialAction (return [MoveObject rSelf Library Battlefield])
    }
  , _staticKeywordAbilities = []
  , _continuousEffects = []
  , _activatedAbilities = []
  , _triggeredAbilities = []
  , _replacementEffects = []
  }

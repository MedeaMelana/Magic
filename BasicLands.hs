{-# LANGUAGE OverloadedStrings #-}

module BasicLands where

import Predicates
import Types
import Utils

import Control.Applicative
import Data.Boolean
import Data.Label.PureM
import Data.Monoid
import Data.Set as Set
import Data.String


plains, island, swamp, mountain, forest :: Card
plains   = mkBasicLandCard Plains   White
island   = mkBasicLandCard Island   Blue
swamp    = mkBasicLandCard Swamp    Black
mountain = mkBasicLandCard Mountain Red
forest   = mkBasicLandCard Forest   Green

mkBasicLandCard :: LandType -> Color -> Card
mkBasicLandCard ty color = Card $ \ts rOwner ->
  Object
  { _name = Just (fromString (show ty))
  , _colors = mempty
  , _types = basicType <> objectType ty
  , _zone = Library
  , _owner = rOwner
  , _controller = rOwner
  , _timestamp = ts
  , _counters = []

  , _tapStatus = Nothing

  , _power = Nothing
  , _toughness = Nothing
  , _damage = Nothing

  , _play = defaultSpecialPlay
  , _staticKeywordAbilities = []
  , _continuousEffects = []
  , _activatedAbilities = [tapToAddMana (Just color)]
  , _triggeredAbilities = []
  , _replacementEffects = []
  }

defaultSpecialPlay :: Ability
defaultSpecialPlay rSource rActivator = ClosedAbility
  { _available = checkObject rSource (isControlledBy rActivator &&* isInZone Hand)
  , _manaCost = mempty
  , _additionalCosts = []
  , _effect = SpecialAction (return [MoveObject rSource Library Battlefield])
  }

tapToAddMana :: Maybe Color -> Ability
tapToAddMana mc rSource rActivator = ClosedAbility
  { _available = checkObject rSource (isControlledBy rActivator &&* isInZone Battlefield)
  , _manaCost = mempty
  , _additionalCosts = []
  , _effect = SpecialAction (return [AddToManaPool rActivator mc])
  }

checkObject :: Ref Object -> (Object -> Bool) -> View Bool
checkObject ref ok = ok . (! ref) <$> asks objects

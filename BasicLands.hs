{-# LANGUAGE OverloadedStrings #-}

module BasicLands where

import Core
import Labels
import Predicates
import Utils

import Control.Applicative
import Data.Label.PureM
import Data.Monoid
import Data.String


plains, island, swamp, mountain, forest :: Card
plains   = mkBasicLandCard Plains   White
island   = mkBasicLandCard Island   Blue
swamp    = mkBasicLandCard Swamp    Black
mountain = mkBasicLandCard Mountain Red
forest   = mkBasicLandCard Forest   Green

mkBasicLandCard :: LandSubtype -> Color -> Card
mkBasicLandCard ty color = mkCard $ do
  name               =: Just (fromString (show ty))
  types              =: basicType <> objectType ty
  play               =: Just playLand
  activatedAbilities =: [tapToAddMana (Just color)]

playLand :: Ability
playLand rSource rActivator = ClosedAbility
  { _available =
      case rSource of
        (Hand _, _) -> checkObject rSource (isControlledBy rActivator)
        _           -> return False
  , _manaCost = mempty
  , _additionalCosts = []
  , _effect = SpecialAction (return [WillSimpleEffect (PlayLand rSource)])
  }

tapToAddMana :: Maybe Color -> Ability
tapToAddMana mc rSource rActivator = ClosedAbility
  { _available =
      case rSource of
        (Battlefield, _) -> checkObject rSource (isControlledBy rActivator)
        _                -> return False
  , _manaCost = mempty
  , _additionalCosts = []
  -- TODO require cost: tap self
  , _effect = SpecialAction (return [WillSimpleEffect (AddToManaPool rActivator mc)])
  }

checkObject :: ObjectRef -> (Object -> Bool) -> View Bool
checkObject (rZone, i) ok = ok <$> asks (compileZoneRef rZone .^ listEl i)

{-# LANGUAGE OverloadedStrings #-}

module BasicLands where

import Labels
import Predicates
import Types
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

mkBasicLandCard :: LandType -> Color -> Card
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
  , _effect = SpecialAction (return [MoveObject rSource Battlefield])
  }

tapToAddMana :: Maybe Color -> Ability
tapToAddMana mc rSource rActivator = ClosedAbility
  { _available =
      case rSource of
        (Battlefield, _) -> checkObject rSource (isControlledBy rActivator)
        _                -> return False
  , _manaCost = mempty
  , _additionalCosts = []
  , _effect = SpecialAction (return [AddToManaPool rActivator mc])
  }

checkObject :: ObjectRef -> (Object -> Bool) -> View Bool
checkObject (rZone, i) ok = ok <$> asks (compileZoneRef rZone .^ listEl i)

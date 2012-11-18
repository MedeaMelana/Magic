{-# LANGUAGE OverloadedStrings #-}

module Magic.BasicLands where

import Magic.Core
import Magic.Labels
import Magic.Events
import Magic.ObjectTypes
import Magic.Predicates
import Magic.Utils
import Magic.Types

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
        (Hand _, _) -> do
          control <- checkObject rSource (isControlledBy rActivator)
          ap <- asks activePlayer
          step <- asks activeStep
          return (control && ap == rActivator && step == MainPhase)
        _           -> return False
  , _manaCost = mempty
  , _additionalCosts = []
  , _effect = SpecialAction ((:[]) <$> view (willMoveToBattlefield rSource))
  }

tapToAddMana :: Maybe Color -> Ability
tapToAddMana mc rSource rActivator = ClosedAbility
  { _available =
      case rSource of
        (Battlefield, _) -> checkObject rSource (isControlledBy rActivator)
        _                -> return False
  , _manaCost = mempty
  , _additionalCosts = [TapSpecificPermanentCost rSource]
  , _effect = SpecialAction (return [Will (AddToManaPool rActivator mc)])
  }

checkObject :: ObjectRef -> (Object -> Bool) -> View Bool
checkObject (rZone, i) ok = ok <$> asks (compileZoneRef rZone .^ listEl i)

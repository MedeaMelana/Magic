{-# LANGUAGE OverloadedStrings #-}

module Magic.BasicLands where

import Magic

import Control.Applicative
import Control.Monad (void)
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
  types              =: basicType <> landTypes [ty]
  play               =: Just playLand
  activatedAbilities =: [tapToAddMana (Just color)]

playLand :: ActivatedAbility
playLand = ActivatedAbility
  { available = \rSource rActivator ->
      case rSource of
        (Hand _, _) -> do
          control <- checkObject rSource (isControlledBy rActivator)
          stackEmpty <- isStackEmpty
          ap <- asks activePlayer
          step <- asks activeStep
          n <- countLandsPlayedThisTurn (== rActivator)

          return (control && ap == rActivator && step == MainPhase && stackEmpty && n < 1)
        _           -> return False
  , manaCost = mempty
  , tapCost = NoTapCost
  , effect = \rSource rActivator -> void (executeEffect (Will (PlayLand rActivator rSource)))
  , isManaAbility = False
  }

countLandsPlayedThisTurn :: (PlayerRef -> Bool) -> View Int
countLandsPlayedThisTurn f = length . filter isPlayLand <$> asks turnHistory
  where
    isPlayLand (Did (PlayLand p _))  = f p
    isPlayLand _                     = False


tapToAddMana :: Maybe Color -> ActivatedAbility
tapToAddMana mc = ActivatedAbility
  { available = \rSource rActivator ->
      case rSource of
        (Battlefield, _) -> checkObject rSource (isControlledBy rActivator)
        _                -> return False
  , manaCost = mempty
  , tapCost = TapCost
  , effect = \_rSource rActivator -> void (executeEffect (Will (AddToManaPool rActivator [mc])))
  , isManaAbility = True
  }

checkObject :: ObjectRef -> (Object -> Bool) -> View Bool
checkObject (rZone, i) ok = ok <$> asks (compileZoneRef rZone .^ listEl i)

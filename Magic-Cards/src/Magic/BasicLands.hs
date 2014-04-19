{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

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

playLand :: Activation
playLand = Activation
  { available = \rSource rActivator ->
      case rSource of
        (Some (Hand _), _) -> do
          control <- checkObject rSource (isControlledBy rActivator)
          stackEmpty <- isStackEmpty
          ap <- asks activePlayer
          step <- asks activeStep
          n <- countLandsPlayedThisTurn (== rActivator)

          return (control && ap == rActivator && step == MainPhase && stackEmpty && n < 1)
        _           -> return False
  , manaCost = mempty
  , effect = \rSource rActivator -> void (executeEffect (Will (PlayLand rActivator rSource)))
  }

countLandsPlayedThisTurn :: (PlayerRef -> Bool) -> View Int
countLandsPlayedThisTurn f = length . filter isPlayLand <$> asks turnHistory
  where
    isPlayLand (Did (PlayLand p _))  = f p
    isPlayLand _                     = False


tapToAddMana :: Maybe Color -> ActivatedAbility
tapToAddMana mc = ActivatedAbility
  { abilityActivation = Activation
    { available = \rSource rActivator ->
        case rSource of
          (Some Battlefield, _) ->
            checkObject rSource (isControlledBy rActivator)
          _ -> return False
    , manaCost = mempty
    , effect = \_rSource rActivator -> void (executeEffect (Will (AddToManaPool rActivator [mc])))
    }
  , abilityType = ManaAb
  , tapCost = TapCost
  }

checkObject :: SomeObjectRef -> (Object -> Bool) -> View Bool
checkObject r ok = ok <$> asks (objectBase r)

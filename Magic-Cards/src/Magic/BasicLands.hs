{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Magic.BasicLands where

import Magic

import Control.Applicative
import Control.Monad (void)
import Data.Label.Monadic ((=:), asks)
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
  play               =: Just playObject
  activatedAbilities =: [tapToAddMana (Just color)]

tapToAddMana :: Maybe Color -> ActivatedAbility
tapToAddMana mc = ActivatedAbility
  { abilityActivation = Activation
    { timing = instantSpeed
    , available = availableFromBattlefield
    , manaCost = mempty
    , effect = \_rSource you ->
        void (executeEffect (Will (AddToManaPool you [mc])))
    }
  , abilityType = ManaAb
  , tapCost = TapCost
  }

checkObject :: SomeObjectRef -> (Object -> Bool) -> View Bool
checkObject r ok = ok <$> asks (objectBase r)

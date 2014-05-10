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
  { abilityType = ManaAb
  , tapCost = TapCost
  , abilityActivation = defaultActivation
    { effect = \_rSource you ->
        void (executeEffect (Will (AddToManaPool you [mc])))
    }
  }

checkObject :: SomeObjectRef -> (Object -> Bool) -> View Bool
checkObject r ok = ok <$> asks (objectBase r)

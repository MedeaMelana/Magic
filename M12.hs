{-# LANGUAGE OverloadedStrings #-}

module M12 where

import Types

import Control.Monad.State
import qualified Data.IntMap as IntMap
import Data.Text (Text)


shock :: Card
shock = mkInstant "Shock" [PayMana [Just Red]] $ do
  t <- targetOne (const True)
  get

mkInstant :: Text -> [Cost] -> Interact Game -> Card
mkInstant name cost effect = Card
  { enterGame = \rOwner rSelf -> Object
    { name = Just name
    , group = Spell Instant
    , zone = Library
    , owner = rOwner
    , controller = rOwner
    , abilities = []
    , play = ActivatedAbility
      { available = \game ->
          let ObjectEntity self = entities game IntMap.! rSelf
           in zone self == Hand
      , cost = cost
      , effect = undefined
      }
    }
  }

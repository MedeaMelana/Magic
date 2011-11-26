{-# LANGUAGE OverloadedStrings #-}

module M12 where

import Types

import Control.Monad.Reader
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text


shock :: Card
shock = mkInstant "Shock" [PayMana [Just Red]] $ do
  t <- targetOne (const True)
  ask

mkInstant :: MonadInteract m => Text -> [Cost] -> m Game -> Card
mkInstant name cost effect = Card
  { enterGame = \rOwner rSelf -> Object
    { name = Just name
    , group = Spell (Instant Set.empty) (Set.empty)
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

{-# LANGUAGE TypeOperators #-}

module Magic.Utils (mkCard, countCountersOfType) where

import qualified Magic.IdList as IdList
import Magic.Types

import Control.Monad.State (State, execState)
import Data.Label.Pure
import Data.List (sortBy)
import Data.Monoid (mempty)
import Data.Ord (comparing)


mkCard :: State Object () -> Card
mkCard f = Card (\ts rOwner -> execState f (object ts rOwner))

player :: Player
player = Player
  { _life            = 20
  , _manaPool        = []
  , _prestack        = []
  , _library         = IdList.empty
  , _hand            = IdList.empty
  , _graveyard       = IdList.empty
  , _maximumHandSize = Just 7
  , _failedCardDraw  = False
  }

object :: Timestamp -> PlayerRef -> Object
object ts rOwner = Object
  { _name = Nothing
  , _colors = mempty
  , _types = mempty
  , _owner = rOwner
  , _controller = rOwner
  , _timestamp = ts
  , _counters = mempty

  , _tapStatus = Nothing
  , _stackItem = Nothing

  , _power = Nothing
  , _toughness = Nothing
  , _damage = Nothing
  , _deathtouched = False

  , _play = Nothing
  , _staticKeywordAbilities = []
  , _continuousEffects = []
  , _activatedAbilities = []
  , _triggeredAbilities = []
  , _replacementEffects = []
  }


countCountersOfType :: CounterType -> Object -> Int
countCountersOfType ty o = length (filter (== ty) (get counters o))

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

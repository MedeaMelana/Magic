{-# LANGUAGE TypeOperators #-}

module Magic.Utils (mkCard, emptyObject, countCountersOfType, sortOn, textShow) where

import Magic.Types

import Control.Monad.State (State, execState)
import Data.Label.Pure
import Data.List (sortBy)
import Data.Monoid (mempty)
import Data.Ord (comparing)
import Data.Text (Text, pack)


mkCard :: State Object () -> Card
mkCard f = Card (\ts rOwner -> execState f (emptyObject ts rOwner))

emptyObject :: Timestamp -> PlayerRef -> Object
emptyObject ts rOwner = Object
  { _name = Nothing
  , _colors = mempty
  , _types = mempty
  , _owner = rOwner
  , _controller = rOwner
  , _timestamp = ts
  , _counters = mempty

  , _tapStatus = Nothing
  , _stackItem = Nothing

  , _pt = Nothing
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

textShow :: Show a => a -> Text
textShow = pack . show
{-# LANGUAGE TypeOperators #-}

module Magic.Utils (mkCard, emptyObject, countCountersOfType, sortOn, textShow, deleteAtIndex, gor, gand, count) where

import Magic.Types

import Control.Monad.State (State, execState)
import Data.Boolean (Boolean(..))
import Data.Label.Pure
import Data.List (sortBy)
import Data.Monoid (mempty)
import Data.Ord (comparing)
import Data.Text (Text, pack)


mkCard :: State Object () -> Card
mkCard f = Card (execState f . emptyObject 0)

emptyObject :: Timestamp -> PlayerRef -> Object
emptyObject t rOwner = Object
  { _name = Nothing
  , _colors = mempty
  , _types = mempty
  , _owner = rOwner
  , _controller = rOwner
  , _timestamp = t
  , _counters = mempty

  , _pt = Nothing
  , _loyalty = Nothing

  , _play = Nothing
  , _staticKeywordAbilities = []
  , _layeredEffects = []
  , _activatedAbilities = []
  , _triggeredAbilities = mempty
  , _replacementEffects = []

  , _temporaryEffects = []
  }


countCountersOfType :: CounterType -> Object -> Int
countCountersOfType ty o = length (filter (== ty) (get counters o))

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

textShow :: Show a => a -> Text
textShow = pack . show

deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex i xs = ys ++ zs
  where
    (ys, _:zs) = splitAt i xs

gor :: Boolean b => [b] -> b
gor = foldr (||*) false

gand :: Boolean b => [b] -> b
gand = foldr (&&*) true

count :: [a] -> (a -> Bool) -> Int
count xs ok = length (filter ok xs)

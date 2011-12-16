{-# LANGUAGE OverloadedStrings #-}

module M12 where

import Types
import Labels
import Predicates

import Control.Monad (when)
import Data.Boolean
import Data.Label.MaybeM
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)


doomblade :: Card
doomblade =
  mkInstant "Doomblade" [PayMana [Just Black, Nothing]] $
    \rSelf rOwner -> do
      let crit = notB (hasColor Black) &&* isOnBattlefield
      rTarget <- targetCreature crit
      stack rSelf $ do
        o <- gets (objects ^. ref rTarget)
        when (crit o) $ do
          objects ^. ref rTarget ^. zone =: Graveyard

goblinFireslinger :: Card
goblinFireslinger = Card
  { enterWorld = \rOwner rSelf -> Object
    { _name = Just "Goblin Fireslinger"
    , _colors = colorsFromCost cost
    , _group = Permanent
      { _supertypes = Set.empty
      , _permanentTypes = Set.fromList
        [ Creature
          { _creatureTypes = Set.fromList [Goblin, Warrior]
          , _power = 1
          , _toughness = 1
          , _damage = 0
          }
        ]
      }
    , _zone = Library
    , _owner = rOwner
    , _controller = rOwner
    , _abilities = undefined
    , _play = undefined
    , _timestamp = undefined
    , _staticAbilities = []
    , _effects = []
    }
  }
  where
    cost = [PayMana [Just Red]]

stack :: Ref Object -> Magic () -> Magic ()
stack r a = objects ^. ref r ^. zone =: Stack a

target :: (Object -> Bool) -> Magic (Ref Object)
target = undefined

targetCreature :: (Object -> Bool) -> Magic (Ref Object)
targetCreature = undefined

targetPlayer :: (Player -> Bool) -> Magic (Ref Player)
targetPlayer = undefined

mkInstant :: Text -> [Cost] -> (Ref Object -> Ref Player ->
  Magic ()) -> Card
mkInstant name cost effect = Card
  { enterWorld = \rOwner rSelf -> Object
    { _name = Just name
    , _colors = colorsFromCost cost
    , _group = Spell Instant
    , _zone = Library
    , _owner = rOwner
    , _controller = rOwner
    , _abilities = []
    , _play = Action
      { _available = do
          self <- gets (objects ^. ref rSelf)
          return (isInHand self)
      , _cost = cost
      , _effect = undefined
      }
    , _timestamp = undefined
    , _staticAbilities = undefined
    , _effects = undefined
    }
  }

colorsFromCost :: [Cost] -> Set Color
colorsFromCost = Set.fromList . concat . map f
  where
    f (PayMana colors) = catMaybes colors
    f _                = []

{-# LANGUAGE OverloadedStrings #-}

module M12 where

import Types
import Labels
import Predicates
import Utils

import Control.Applicative
import Control.Monad (when)
import Data.Boolean
import qualified Data.IntMap as IntMap
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
        o <- gets (object rTarget)
        when (crit o) $ do
          object rTarget .^ zone =: Graveyard

goblinFireslinger :: Card
goblinFireslinger = Card
  { enterWorld = \timestamp rOwner rSelf -> Object
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
    , _activatedAbilities =
      [ Action
        { _available = \rp ->
            (isOnBattlefield &&* isControlledBy rp) <$>
            gets (object rSelf)
        , _cost = [TapSelf]
        , _effect = do
            rt <- targetPlayer
            stack rSelf $ player rt .^ life .~ subtract 1
        }
      ]
    , _play = Action
      { _available = \rp ->
          (isInHand &&* isControlledBy rp) <$>
          gets (object rSelf)
      , _cost = [PayMana [Just Red]]
      , _effect = stack rSelf $
          object rSelf .^ zone =: Battlefield Untapped
      }
    , _timestamp = timestamp
    , _staticAbilities = []
    , _counters = []
    , _effects = []
    }
  }
  where
    cost = [PayMana [Just Red]]

stack :: Ref Object -> Magic () -> Magic ()
stack r a = move r (Stack a)

move :: Ref Object -> Zone -> Magic ()
move r z = do
  object r .^ zone =: z
  object r .^ effects =: []
  object r .^ counters =: []  -- [121.2]
  stamp >>= puts (object r .^ timestamp)  -- [613.6c]

target :: (Object -> Bool) -> Magic (Ref Object)
target = undefined

targetCreature :: (Object -> Bool) -> Magic (Ref Object)
targetCreature = undefined

targetPlayer :: Magic (Ref Player)
targetPlayer = targetPlayer' (const True)

targetOpponent :: Ref Player -> Magic (Ref Player)
targetOpponent rController = targetPlayer' (\(rp, _) -> rp /= rController)

targetPlayer' :: (WithRef Player -> Bool) -> Magic (Ref Player)
targetPlayer' f = do
  rpps <- IntMap.toList <$> gets players
  choose [ (TargetPlayer rp, rp) | rpp@(rp, _) <- rpps, f rpp ]

mkInstant :: Text -> [Cost] -> (Ref Object -> Ref Player ->
  Magic ()) -> Card
mkInstant name cost effect = Card
  { enterWorld = \timestamp rOwner rSelf -> Object
    { _name = Just name
    , _colors = colorsFromCost cost
    , _group = Spell Instant
    , _zone = Library
    , _owner = rOwner
    , _controller = rOwner
    , _activatedAbilities = []
    , _play = Action
      { _available = \rp ->
        (isInHand &&* isControlledBy rp) <$>
        gets (object rSelf)
      , _cost = cost
      , _effect = stack rSelf effect
      }
    , _timestamp = timestamp
    , _staticAbilities = []
    , _counters = []
    , _effects = []
    }
  }

colorsFromCost :: [Cost] -> Set Color
colorsFromCost = Set.fromList . concat . map f
  where
    f (PayMana colors) = catMaybes colors
    f _                = []

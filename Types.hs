{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Prelude hiding ((.), id)

import Control.Arrow (ArrowZero(..), ArrowChoice(..), arr, returnA)
import Control.Category ((.), id)
import Control.Monad.State

import Data.Label (mkLabels, Lens(..))
import Data.Label.Abstract (lens)
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text


type Bag = []

type Ref a = Int
type RefMap = IntMap


-- | Current game situation.
data World = World
  { _objects      :: RefMap Object
  , _players      :: RefMap Player
  , _activePlayer :: Ref Player
  , _activeStep   :: Step
  }

data Step
  -- Beginning phase
  = UntapStep
  | UpkeepStep
  | DrawStep
  
  -- Main phase
  | MainPreCombatPhase
  
  -- Combat phase
  | BeginningOfCombatStep
  | DeclareAttackersStep
  | DeclareBlockersStep
  | CombatDamageStep
  | EndOfCombatStep
  
  -- Main phase
  | MainPostCombatPhase
  
  -- End phase
  | EndOfTurnStep
  | CleanupStep
  
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Player = Player
  { _life     :: Int
  , _manaPool :: Bag (Maybe Color)
  } deriving (Eq, Ord, Show)

data Card = Card
  { enterWorld :: Ref Player -> Ref Object -> Object
  }


-- Objects

data Object = Object
  { _name       :: Maybe Text
  , _group      :: Group
  , _zone       :: Zone
  , _owner      :: Ref Player
  , _controller :: Ref Player
  , _abilities  :: [ActivatedAbility]
  , _play       :: ActivatedAbility
  }

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Zone = Library | Hand | Stack { resolution :: Interact () }
  | Battlefield TapStatus | Graveyard | Exile

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Group
  = Spell { _spellType :: SpellType }
  | Permanent
    { _supertypes     :: Set Supertype
    , _permanentTypes :: Set PermanentType }
  deriving (Eq, Ord, Show)

data SpellType = Instant | Sorcery
  deriving (Eq, Ord, Show)

data Supertype = Basic | Legendary
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PermanentType
  = Artifact      { _artifactTypes     :: Set ArtifactType }
  | Creature      { _creatureTypes     :: Set CreatureType
                  , _power             :: Int
                  , _toughness         :: Int }
  | Enchantment   { _enchantmentTypes  :: Set EnchantmentType }
  | Land          { _landTypes         :: Set LandType }
  | Planeswalker  { _planeswalkerTypes :: Set PlaneswalkerType }
  deriving (Eq, Ord, Show)

data ArtifactType = Equipment
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CreatureType
  -- Races
  = Boar
  | Human
  | Spirit
  | Treefolk
  | Insect
  | Spider
  | Devil
  
  -- Roles
  | Warrior
  | Shaman
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data EnchantmentType = Aura | Curse
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data LandType = Plains | Island | Swamp | Mountain | Forest | Locus
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PlaneswalkerType = Chandra | Elspeth | Garruk | Gideon | Jace
  | Koth | Liliana | Sorin | Tezzeret | Venser | Karn 
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


-- Actions

data ActivatedAbility = ActivatedAbility
  { _available :: World -> Bool
  , _cost      :: [Cost]
  , _effect    :: Interact ()
  }

data Cost
  = PayMana (Bag (Maybe Color))
  | PayLife Int
  | SacrificeCost (Object -> Bool)
  | ExileCost (Object -> Bool)


data Interact :: * -> * where
  Return   :: a -> Interact a
  Bind     :: Interact a -> (a -> Interact b) -> Interact b
  GetWorld :: Interact World
  PutWorld :: World -> Interact ()
  Choose   :: [Choice a] -> Interact a

choose :: [Choice a] -> Interact a
choose = Choose

instance Monad Interact where
  return = Return
  (>>=)  = Bind

instance MonadState World Interact where
  get = GetWorld
  put = PutWorld

data Choice a
  = TargetPlayer (Ref Player) a
  | TargetObject (Ref Object) a
  | Custom Text a  -- with explanation

$(mkLabels [''World, ''Player, ''Object, ''Zone, ''Group, ''PermanentType, ''ActivatedAbility])

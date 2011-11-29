{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Control.Monad.State
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
  { objects      :: RefMap Object
  , players      :: RefMap Player
  , activePlayer :: Ref Player
  , activeStep   :: Step
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
  { life     :: Int
  , manaPool :: Bag (Maybe Color)
  } deriving (Eq, Ord, Show)

data Card = Card
  { enterWorld :: Ref Player -> Ref Object -> Object
  }


-- Objects

data Object = Object
  { name       :: Maybe Text
  , group      :: Group
  , zone       :: Zone
  , owner      :: Ref Player
  , controller :: Ref Player
  , abilities  :: [ActivatedAbility]
  , play       :: ActivatedAbility
  }

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Zone = Library | Hand | Stack
  | Battlefield TapStatus | Graveyard | Exile
  deriving (Eq, Ord, Show)

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Group
  = Spell SpellType
  | Permanent (Set Supertype) (Set PermanentType)
  deriving (Eq, Ord, Show)

data SpellType = Instant | Sorcery
  deriving (Eq, Ord, Show)

data Supertype = Basic | Legendary
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PermanentType
  = Artifact      (Set ArtifactType)
  | Creature      (Set CreatureType) Power Toughness
  | Enchantment   (Set EnchantmentType)
  | Land          (Set LandType)
  | Planeswalker  (Set PlaneswalkerType)
  deriving (Eq, Ord, Show)

type Power = Int
type Toughness = Int

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
  { available :: World -> Bool
  , cost      :: [Cost]
  , effect    :: World -> World
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

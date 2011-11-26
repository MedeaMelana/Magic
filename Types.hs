{-# LANGUAGE FlexibleContexts #-}

module Types where

import Control.Monad.Reader
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
data Game = Game
  { entities     :: RefMap Entity
  , activePlayer :: Ref Entity
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

data Entity
  = PlayerEntity Player
  | ObjectEntity Object

data Player = Player
  { life     :: Int
  , manaPool :: Bag (Maybe Color)
  } deriving (Eq, Ord, Show)

data Zone = Library | Hand | Stack
  | Battlefield TapStatus | Graveyard | Exile
  deriving (Eq, Ord, Show)

data Card = Card
  { enterGame :: Ref Player -> Ref Object -> Object
  }

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

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Group
  = Spell SpellType (Set SpellSubtype)
  | Permanent (Set Supertype) (Set PermanentType)
  deriving (Eq, Ord, Show)

data Cost
  = PayMana (Bag (Maybe Color))
  | PayLife Int
  | SacrificeCost (Object -> Bool)
  | ExileCost (Object -> Bool)


-- Spells

data SpellType
  = Instant (Set InstantType)
  | Sorcery
  deriving (Eq, Ord, Show)

data SpellSubtype = Arcane
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data InstantType = Trap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)



-- Permanents

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

data ArtifactType = Contraption | Fortification | Equipment
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

data EnchantmentType = Aura | Curse | Shrine
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data LandType
  = Plains | Island | Swamp | Mountain | Forest
  | Desert | Lair | Locus | Urza's | Mine | PowerPlant | Tower
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PlaneswalkerType
  = Ajani | Bolas | Chandra | Elspeth | Garruk | Gideon | Jace | Koth
  | Liliana | Nissa | Sarkhan | Sorin | Tezzeret | Venser | Karn 
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


-- Actions

data ActivatedAbility = ActivatedAbility
  { available :: Game -> Bool
  , cost      :: [Cost]
  , effect    :: Game -> Game
  }


class MonadReader Game m => MonadInteract m where
  choose :: [Choice a] -> m a

targetOne :: (Entity -> Bool) -> m Entity
targetOne = undefined

data Choice a
  = TargetEntity (Ref Entity) a
  | Custom Text a  -- with explanation

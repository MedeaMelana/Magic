{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Monad.State
import Data.Label (mkLabels)
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Text (Text)


type Bag = []

type Ref a = Int
type RefMap = IntMap
type RefSet a = Set (Ref a)
type WithRef a = (Ref a, a)


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
  | PrecombatMainPhase
  
  -- Combat phase
  | BeginningOfCombatStep
  | DeclareAttackersStep
  | DeclareBlockersStep
  | CombatDamageStep
  | EndOfCombatStep
  
  -- Main phase
  | PostcombatMainPhase
  
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
  , _colors     :: Set Color
  , _group      :: Group
  , _zone       :: Zone
  , _owner      :: Ref Player
  , _controller :: Ref Player
  , _play       :: Action
  , _activatedAbilities  :: [Action]
  , _timestamp  :: Timestamp
  -- , _triggeredAbilities :: Event -> Magic ()
  , _staticAbilities :: Bag StaticAbility
  , _counters   :: Bag Counter
  , _effects    :: [World -> World]  -- cleared when object changes zone
  }

type Timestamp = Int

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Zone = Library | Hand | Stack { _resolve :: Magic () }
  | Battlefield { _tapStatus :: TapStatus } | Graveyard | Exile

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Counter
  = Charge
  | Plus1Plus1
  | Minus1Minus1
  | Poison
  | Hatchling

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
                  , _toughness         :: Int
                  , _damage            :: Int }
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
  | Goblin
  
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

data Action = Action
  { _available :: Ref Player -> Magic Bool  -- check for cost is implied
  , _cost      :: [Cost]
  , _effect    :: Magic ()
  }

data Cost
  = PayMana (Bag (Maybe Color))
  | PayLife Int
  | TapSelf
  | Sacrifice (Object -> Bool)
  | SacrificeSpecific (Ref Object)
  | ExileCost (Object -> Bool)
  | RemoveCounter (Ref Object) Counter

data StaticAbility
  = Flying
  | Intimidate
  -- | CannotBeBlockedBy (Object -> Bool)
  | Deathtouch
  | Defender
  | DoubleStrike
  | Enchant
  | FirstStrike
  | Flash
  | Haste
  | Hexproof
  | Lifelink
  | Protection (Object -> Bool)  -- spell, or ability's source
  | Reach
  | Shroud
  | Trample
  | Vigilance
  | Flashback [Cost]
  | Bloodthirst Int
  | Infect

-- data TriggeredAbility
--   = BattleCry
--   | LivingWeapon

data Effect = Effect
  { _layer       :: Layer
  , _efTimestamp :: Timestamp
  , _efEffect    :: Magic ()
  }

data Layer
  = Layer1       -- copy effects
  | Layer2       -- control-changing effects
  | Layer3       -- text-changing effects
  | Layer4       -- type-chaning effects
  | Layer5       -- color-changing effects
  | Layer6       -- ability-adding and ability-removing effects
  | Layer7a      -- p/t from characteristic-defining abilities
  | Layer7b      -- set p/t
  | Layer7c      -- modify p/t
  | Layer7d      -- p/t counters
  | Layer7e      -- switch p/t
  | LayerPlayer  -- player-affecting effects
  | LayerRules   -- rules-affecting effects
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Magic :: * -> * where
  Return   :: a -> Magic a
  Bind     :: Magic a -> (a -> Magic b) -> Magic b
  GetWorld :: Magic World
  PutWorld :: World -> Magic ()
  Choose   :: [(Choice, a)] -> Magic a
  Fail     :: Magic a
  Plus     :: Magic a -> Magic a -> Magic a

choose :: [(Choice, a)] -> Magic a
choose = Choose

instance Functor Magic where
  fmap = liftM

instance Applicative Magic where
  pure  = return
  (<*>) = ap

instance Monad Magic where
  return = Return
  (>>=)  = Bind

instance MonadState World Magic where
  get = GetWorld
  put = PutWorld

instance MonadPlus Magic where
  mzero = Fail
  mplus = Plus

data Choice
  = TargetPlayer (Ref Player)
  | TargetObject (Ref Object)

$(mkLabels [''World, ''Player, ''Object, ''Zone, ''Group,
  ''PermanentType, ''Action])

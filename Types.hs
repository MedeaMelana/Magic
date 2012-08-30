{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Label (mkLabels)
import Data.IntMap (IntMap)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)


type Bag = []

newtype Ref a = Ref Int
type RefMap = IntMap
type RefSet a = Set (Ref a)
type WithRef a = (Ref a, a)


-- | Current game situation.
data World = World
  { _objects      :: RefMap Object
  , _players      :: RefMap Player
  , _activePlayer :: Ref Player
  , _priority     :: Ref Player
  , _activeStep   :: Step
  --, _prestack     :: [Special StackedEffect]
  --, _stack        :: [StackedEffect]
  , _time         :: Timestamp
  }


-- Steps and phases

data Step
  = BeginningPhase BeginningStep
  | PrecombatMainPhase
  | CombatPhase CombatStep
  | PostcombatMainPhase
  | EndPhase EndStep
  deriving (Eq, Ord, Show, Read)

data BeginningStep
  = UntapStep
  | UpkeepStep
  | DrawStep
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CombatStep
  = BeginningOfCombatStep
  | DeclareAttackersStep
  | DeclareBlockersStep
  | CombatDamageStep
  | EndOfCombatStep
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data EndStep
  = EndOfTurnStep
  | CleanupStep
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Player = Player
  { _life     :: Int
  , _manaPool :: Bag (Maybe Color)
  } deriving (Eq, Ord, Show)


-- Objects

data Card = Card
  -- timestamp, owner (and controller), new ref
  { enterWorld :: Timestamp -> Ref Player -> Ref Object -> Object
  }

data Object = Object
  { _name       :: Maybe Text
  , _colors     :: Set Color
  , _group      :: Group
  , _zone       :: Zone
  , _owner      :: Ref Player
  , _controller :: Ref Player
  , _timestamp  :: Timestamp
  , _counters   :: Bag CounterType

  -- for permanents on the battlefield
  , _tapStatus :: Maybe TapStatus

  -- for spells on the stack
  --, _pendingEffect :: Maybe StackedEffect

  -- for creatures on the battlefield
  , _power         :: Maybe Int
  , _toughness     :: Maybe Int
  , _damage        :: Maybe Int
  , _mustBeBlocked :: Maybe Bool
  , _mustAttack    :: Maybe Bool

  , _indestructible    :: Bool

  , _play                   :: Action
  , _staticKeywordAbilities :: Bag StaticKeywordAbility
  , _continuousEffects      :: [ContinuousEffect]  -- special form of static ability
  , _activatedAbilities     :: [Action]
  --, _triggeredAbilities     :: [Event -> Special StackedEffect]
  }

type Timestamp = Int

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Zone = Library | Hand | Stack | Battlefield | Graveyard | Exile
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CounterType
  = Charge | Plus1Plus1 | Minus1Minus1 | Poison | Hatchling | Loyalty
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Group
  = Spell { _spellType   :: SpellType }
  | Permanent
    { _supertypes        :: Set Supertype
    , _artifactTypes     :: Maybe (Set ArtifactType)
    , _creatureTypes     :: Maybe (Set CreatureType)
    , _enchantmentTypes  :: Maybe (Set EnchantmentType)
    , _landTypes         :: Maybe (Set LandType)
    , _planeswalkerTypes :: Maybe (Set PlaneswalkerType)
    }
  deriving (Eq, Ord, Show)

data SpellType = Instant | Sorcery
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Supertype = Basic | Legendary
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
  { _available :: Ref Player -> View Bool  -- check for cost is implied
  , _cost      :: Cost
  --, _effect    :: Special StackedEffect
  }

data Cost = Cost
  { payColoredMana      :: Bag Color
  , payGenericMana      :: Int
  , tapPermanents       :: [WithRef Object -> Bool]
  , sacrificePermanents :: [WithRef Object -> Bool]
  , exileObjects        :: [WithRef Object -> Bool]
  , discardCards        :: Int
  , removeCounters      :: [(Int, CounterType)]
  }

instance Monoid Cost where
  mempty = Cost [] 0 [] [] [] 0 []
  c1 `mappend` c2 = Cost
    { payColoredMana      = payColoredMana      c1 ++ payColoredMana      c2
    , payGenericMana      = payGenericMana      c1 +  payGenericMana      c2
    , tapPermanents       = tapPermanents       c1 ++ tapPermanents       c2
    , sacrificePermanents = sacrificePermanents c1 ++ sacrificePermanents c2
    , exileObjects        = exileObjects        c1 ++ exileObjects        c2
    , discardCards        = discardCards        c1 +  discardCards        c2
    , removeCounters      = removeCounters      c1 ++ removeCounters      c2
    }

data StaticKeywordAbility
  = Bloodthirst Int
  | Deathtouch
  | Defender
  | DoubleStrike
  | Enchant
  | FirstStrike
  | Flash
  | Flashback Cost
  | Flying
  | Haste
  | Hexproof
  | Infect
  | Intimidate
  | Lifelink
  | ProtectionFromColor Color
  | Reach
  | Shroud
  | Trample
  | Vigilance

data ContinuousEffect = ContinuousEffect
  { _layer       :: Layer
  , _efTimestamp :: Timestamp
  , _efEffect    :: World -> World
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

-- | Events triggered abilities watch for.
data Event
  = OneShotEffectEvent OneShotEffect

  -- Keyword actions [701]
  | ActivateAbility (Ref Object) Int  -- index of ability
  | CastSpell (Ref Player) (Ref Object)  -- controller, spell
  | Counter (Ref Object) (Ref Object)  -- source (spell or ability), target
  | PlayLand (Ref Object)
  | RegeneratePermanent (Ref Object)
  | RevealCard (Ref Object)
  | ChangeStep Step Step  -- old step, new step
  | LoseGame (Ref Player)

data OneShotEffect
  = AdjustLife (Ref Player) Int
  | DamageObject (Ref Object) (Ref Object) Int Bool Bool  -- source, creature/planeswalker, amount, combat damage?, preventable?
  | DamagePlayer (Ref Object) (Ref Player) Int Bool Bool  -- source, player, amount, combat damage?, preventable?
  | ShuffleLibrary
  -- | ReorderLibraryCards
  | DrawCard  -- Drawing is special [120.5]
  | DestroyPermanent (Ref Object) Bool  -- target, preventable? -- Destruction is special [701.6b]
  | MoveObject (Ref Object) Zone Zone
  | TapPermanent (Ref Object)
  | UntapPermanent (Ref Object)
  | AddCounter (Ref Object) CounterType
  | RemoveCounter (Ref Object) CounterType
  | CreateObject Object  -- create a token, emblem or spell
  | AddToManaPool (Ref Player) (Bag (Maybe Color))
  | AttachPermanent (Ref Object) (Maybe (Ref Object)) (Maybe (Ref Object))  -- aura/equipment, old target, new target
  | RemoveFromCombat (Ref Object)

data Choice
  = ChoosePlayer (Ref Player)
  | ChooseObject (Ref Object)
  | ChooseColor Color
  | ChooseNumber Int
  | Pass
  | Concede



-- Monads

type ViewT = ReaderT World
type View = ViewT Identity

data Special a

instance Functor Special
instance Applicative Special
instance Monad Special
instance MonadReader World Special
-- instance MonadPrompt Special

data Prompt a
  = PromptReturn a
  | PromptAsk (Ref Player)

data Target
  = TargetPlayer (Ref Player)
  | TargetObject (Ref Object)

$(mkLabels [''World, ''Player, ''Object, ''Zone, ''Group, ''Action])

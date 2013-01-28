{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Magic.Types (
    -- * Data structures
    Bag,

    -- * Reference types
    PlayerRef, ObjectRef, ActivatedAbilityRef, ZoneRef(..),

    -- * World
    World(..), players, activePlayer, activeStep, time, turnStructure, exile, battlefield, stack, command,

    -- * Turn structure
    Step(..), BeginningStep(..), CombatStep(..), EndStep(..),

    -- * Players
    Player(..), life, manaPool, prestack, library, hand, graveyard, maximumHandSize, failedCardDraw,

    -- * Objects
    Card(..), Deck,
    Object(..),
      name, colors, types, owner, controller, timestamp, counters,
      tapStatus,
      stackItem,
      power, toughness, damage, deathtouched,
      play, staticKeywordAbilities, continuousEffects, activatedAbilities, triggeredAbilities, replacementEffects,

    -- * Object properties
    Timestamp, Color(..), TapStatus(..), CounterType(..),

    -- * Object types
    ObjectTypes(..), supertypes, artifactSubtypes, creatureSubtypes,
      enchantmentSubtypes, instantSubtypes, landSubtypes,
      planeswalkerSubtypes, sorcerySubtypes,
    Supertype(..), ArtifactSubtype(..), CreatureSubtype(..),
    EnchantmentSubtype(..), SpellSubtype(..), LandSubtype(..),
    PlaneswalkerSubtype(..),

    -- * Abilities
    Ability,
    ClosedAbility(..), available, manaCost, additionalCosts, effect, isManaAbility,
    StackItem, ManaPool, AdditionalCost(..),
    StaticKeywordAbility(..), ContinuousEffect(..), Layer(..),
    ReplacementEffect,
    PriorityAction(..), PayManaAction(..),

    -- * Events
    Event(..), OneShotEffect(..), SimpleOneShotEffect(..),

    -- * Targets
    Target(..), TargetList(..),

    -- * Monads
    ViewT(..), View, Magic, Engine,
    view,
    Interact(..), Question(..)
  ) where

import Magic.IdList (Id, IdList)

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Random (RandT, StdGen)
import Control.Monad.Reader
import Control.Monad.State (StateT)
import qualified Control.Monad.Operational as Operational
import Data.Label (mkLabels)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)



-- DATA STRUCTURES


type Bag = []



-- REFERENCE TYPES


type PlayerRef = Id
type ObjectRef = (ZoneRef, Id)
type ActivatedAbilityRef = (ObjectRef, Int)

data ZoneRef = Library PlayerRef | Hand PlayerRef | Battlefield | Graveyard PlayerRef | Stack | Exile | Command
  deriving (Eq, Ord, Show)



-- WORLD


-- | Current game situation.
data World = World
  { _players       :: IdList Player
  , _activePlayer  :: PlayerRef
  , _activeStep    :: Step
  , _time          :: Timestamp
  , _turnStructure :: [(PlayerRef, [Step])]
  , _exile         :: IdList Object
  , _battlefield   :: IdList Object
  , _stack         :: IdList Object
  , _command       :: IdList Object
  }



-- TURN STRUCTURE


data Step
  = BeginningPhase BeginningStep
  | MainPhase
  | CombatPhase CombatStep
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



-- PLAYERS


data Player = Player
  { _life            :: Int
  , _manaPool        :: Bag (Maybe Color)
  , _prestack        :: [Magic Object]  -- for triggered abilities
  , _library         :: IdList Object
  , _hand            :: IdList Object
  , _graveyard       :: IdList Object
  , _maximumHandSize :: Maybe Int
  , _failedCardDraw  :: Bool  -- [704.5b]
  }



-- OBJECTS


data Card = Card
  -- timestamp, owner (and controller)
  { instantiateCard :: Timestamp -> PlayerRef-> Object
  }

type Deck = [Card]

data Object = Object
  { _name       :: Maybe Text
  , _colors     :: Set Color
  , _types      :: ObjectTypes
  , _owner      :: PlayerRef
  , _controller :: PlayerRef
  , _timestamp  :: Timestamp
  , _counters   :: Bag CounterType

  -- for permanents on the battlefield
  , _tapStatus :: Maybe TapStatus

  -- for spells on the stack
  , _stackItem :: Maybe StackItem

  -- for creatures
  , _power         :: Maybe Int
  , _toughness     :: Maybe Int

  -- for creatures on the battlefield
  , _damage        :: Maybe Int
  , _deathtouched  :: Bool
  --, _mustBeBlocked :: Maybe Bool
  --, _mustAttack    :: Maybe Bool

  --, _indestructible    :: Bool

  , _play                   :: Maybe Ability
  , _staticKeywordAbilities :: Bag StaticKeywordAbility
  , _continuousEffects      :: [ContinuousEffect]  -- special form of static ability
  , _activatedAbilities     :: [Ability]
  , _triggeredAbilities     :: [Event -> Magic [OneShotEffect]]
  , _replacementEffects     :: [ReplacementEffect]
  }



-- OBJECT PROPERTIES


type Timestamp = Int

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CounterType
  = Charge | Plus1Plus1 | Minus1Minus1 | Poison | Hatchling | Loyalty
  deriving (Eq, Ord, Show, Read, Enum, Bounded)



-- OBJECT TYPES


data ObjectTypes = ObjectTypes
  { _supertypes           :: Set Supertype
  , _artifactSubtypes     :: Maybe (Set ArtifactSubtype)
  , _creatureSubtypes     :: Maybe (Set CreatureSubtype)
  , _enchantmentSubtypes  :: Maybe (Set EnchantmentSubtype)
  , _instantSubtypes      :: Maybe (Set SpellSubtype)
  , _landSubtypes         :: Maybe (Set LandSubtype)
  , _planeswalkerSubtypes :: Maybe (Set PlaneswalkerSubtype)
  , _sorcerySubtypes      :: Maybe (Set SpellSubtype)
  } deriving (Eq, Ord, Show)

instance Monoid ObjectTypes where
  mempty = ObjectTypes mempty mempty mempty mempty mempty mempty mempty mempty
  x  `mappend` y = ObjectTypes
    { _supertypes           = _supertypes x           `mappend` _supertypes y
    , _artifactSubtypes     = _artifactSubtypes x     `mappend` _artifactSubtypes y
    , _creatureSubtypes     = _creatureSubtypes x     `mappend` _creatureSubtypes y
    , _enchantmentSubtypes  = _enchantmentSubtypes x  `mappend` _enchantmentSubtypes y
    , _instantSubtypes      = _instantSubtypes x      `mappend` _instantSubtypes y
    , _landSubtypes         = _landSubtypes x         `mappend` _landSubtypes y
    , _planeswalkerSubtypes = _planeswalkerSubtypes x `mappend` _planeswalkerSubtypes y
    , _sorcerySubtypes      = _sorcerySubtypes x      `mappend` _sorcerySubtypes y
    }

data Supertype = Basic | Legendary
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ArtifactSubtype = Equipment
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CreatureSubtype
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

data EnchantmentSubtype = Aura | Curse
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SpellSubtype = Arcane | Trap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data LandSubtype = Plains | Island | Swamp | Mountain | Forest | Locus
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PlaneswalkerSubtype = Chandra | Elspeth | Garruk | Gideon | Jace
  | Koth | Liliana | Sorin | Tezzeret | Venser | Karn 
  deriving (Eq, Ord, Show, Read, Enum, Bounded)



-- ABILITIES


type Ability = ObjectRef -> PlayerRef -> ClosedAbility

data ClosedAbility = ClosedAbility
  { _available       :: View Bool  -- check for cost is implied
  , _manaCost        :: ManaPool
  , _additionalCosts :: [AdditionalCost]
  , _effect          :: Magic [OneShotEffect]
  , _isManaAbility   :: Bool
  }

type StackItem = TargetList Target (Object -> Magic [OneShotEffect])

type ManaPool = Bag (Maybe Color)

data AdditionalCost
  = TapSelf
  | SacrificePermanentCost (Object -> Bool)
  | ExileObjectCost        [ZoneRef] (Object -> Bool)  -- exile matching object from any of the listed zones
  | DiscardCardCost
  | RemoveCounterCost      CounterType

data StaticKeywordAbility
  = Bloodthirst Int
  | Deathtouch
  | Defender
  | DoubleStrike
  | Enchant
  | FirstStrike
  | Flash
  | Flashback ManaPool
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
  deriving (Eq, Ord, Show, Read)

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

type ReplacementEffect = OneShotEffect -> Maybe (Magic [OneShotEffect])

data PriorityAction
  = PlayCard ObjectRef
  | ActivateAbility ActivatedAbilityRef

-- Actions that may be taken when paying a mana cost
data PayManaAction
  = PayManaFromManaPool (Maybe Color)
  | ActivateManaAbility ActivatedAbilityRef



-- EVENTS


-- | Events are caused by various actions in the game. They describe something that has just happened, such as executing a 'OneShotEffect', progressing to the next step or phases, casting spells, et cetera. Events form the input for triggered abilities.
data Event
  = Did SimpleOneShotEffect
  | DidMoveObject ZoneRef ObjectRef  -- old zone, new zone/id

  -- Keyword actions [701]
  | DidActivateAbility ObjectRef Int  -- index of ability
  | DidCastSpell PlayerRef ObjectRef  -- controller, spell
  | DidCounter ObjectRef ObjectRef  -- source (spell or ability), target
  | DidPlayLand ObjectRef
  | DidRevealCard ObjectRef
  | DidBeginStep Step
  | WillEndStep Step

-- | A one-shot effect causes a mutation in the game's state. A value of @OneShotEffect@ describes something that is about to happen. When one-shot effects are executed, they may be replaced or prevented by replacement effects, and cause an 'Event' to be raised, triggering abilities.
data OneShotEffect
  = Will SimpleOneShotEffect
  | WillMoveObject ObjectRef ZoneRef Object  -- current zone/id, new zone, suggested form

-- | A one-shot effect is simple if its fields contain enough information to serve as an 'Event' unchanged, using the 'Did' constructor.
data SimpleOneShotEffect
  = AdjustLife PlayerRef Int
  | DamageObject Object ObjectRef Int Bool Bool  -- source, creature/planeswalker, amount, combat damage?, preventable?
  | DamagePlayer Object PlayerRef Int Bool Bool  -- source, player, amount, combat damage?, preventable?
  | ShuffleLibrary PlayerRef
  -- ReorderLibraryCards
  | DrawCard PlayerRef -- Drawing is special [120.5]
  | DestroyPermanent Id Bool  -- object on battlefield, regenerate allowed?
  | TapPermanent Id  -- object on battlefield
  | UntapPermanent Id  -- object on battlefield
  | AddCounter ObjectRef CounterType
  | RemoveCounter ObjectRef CounterType
  | CreateObject Object  -- create a token, emblem or spell
  | AddToManaPool PlayerRef ManaPool
  | SpendFromManaPool PlayerRef ManaPool
  | AttachPermanent ObjectRef (Maybe ObjectRef) (Maybe ObjectRef)  -- aura/equipment, old target, new target
  | RemoveFromCombat Id
  | PlayLand ObjectRef
  | LoseGame PlayerRef



-- TARGETS


data Target
  = TargetPlayer PlayerRef
  | TargetObject ObjectRef


data TargetList t a where
  Nil  :: a -> TargetList t a
  Snoc :: TargetList t (Target -> a) -> t -> TargetList t a
  Test :: (x -> a) -> (x -> View Bool) -> TargetList t x -> TargetList t a

instance Functor (TargetList t) where
  fmap f (Nil x)        = Nil (f x)
  fmap f (Snoc xs t)    = Snoc (fmap (f .) xs) t
  fmap f (Test g ok xs) = Test (f . g) ok xs

instance Applicative (TargetList t) where
  pure = Nil
  xs <*> Nil b     = fmap ($ b) xs
  xs <*> Snoc ys t = Snoc ((.) <$> xs <*> ys) t
  xs <*> Test f ok ys = Test fst snd ((\g x -> (g (f x), ok x)) <$> xs <*> ys)



-- MONADS


newtype ViewT m a = ViewT { runViewT :: ReaderT World m a }
  deriving (Functor, Applicative, Monad, MonadReader World, MonadTrans)

instance (Monoid a, Monad m) => Monoid (ViewT m a) where
  mempty = return mempty
  ViewT x `mappend` ViewT y = ViewT (liftM2 mappend x y)

type View = ViewT Identity

type Magic = ViewT (Operational.Program Interact)

type Engine = StateT World (RandT StdGen (Operational.Program Interact))

data Interact a where
  Debug       :: Text -> Interact ()
  LogEvent    :: Event -> World -> Interact ()
  AskQuestion :: PlayerRef -> World -> Question a -> Interact a

data Question a where
  AskKeepHand              :: Question Bool
  AskPriorityAction        :: [PriorityAction] -> Question (Maybe PriorityAction)
  AskManaAbility           :: ManaPool -> [PayManaAction] -> Question PayManaAction
  AskTarget                :: [Target] -> Question Target
  AskReorder               :: [a] -> Question [a]
  AskPickReplacementEffect :: [(ReplacementEffect, Magic [OneShotEffect])] -> Question (Pick (ReplacementEffect, Magic [OneShotEffect]))

type Pick a = (a, [a])

view :: View a -> Magic a
view v = ViewT $ ReaderT $ return . runIdentity . runReaderT (runViewT v)

$(mkLabels [''World, ''Player, ''Object, ''ObjectTypes, ''ClosedAbility])

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import IdList (Id, IdList)

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Control.Monad.Operational as Operational
import Data.Label (mkLabels)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)


type Bag = []

type PlayerRef = Id
type ObjectRef = (ZoneRef, Id)


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


-- Steps and phases

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

data Player = Player
  { _life            :: Int
  , _manaPool        :: Bag (Maybe Color)
  , _prestack        :: [Magic StackItem]
  , _library         :: IdList Object
  , _hand            :: IdList Object
  , _graveyard       :: IdList Object
  , _maximumHandSize :: Maybe Int
  , _failedCardDraw  :: Bool  -- [704.5b]
  }


-- Objects

data Card = Card
  -- timestamp, owner (and controller)
  { instantiateCard :: Timestamp -> PlayerRef-> Object
  }

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
  --, _pendingEffect :: Maybe StackedEffect

  -- for creatures on the battlefield
  , _power         :: Maybe Int
  , _toughness     :: Maybe Int
  , _damage        :: Maybe Int
  --, _mustBeBlocked :: Maybe Bool
  --, _mustAttack    :: Maybe Bool

  --, _indestructible    :: Bool

  , _play                   :: Maybe Ability
  , _staticKeywordAbilities :: Bag StaticKeywordAbility
  , _continuousEffects      :: [ContinuousEffect]  -- special form of static ability
  , _activatedAbilities     :: [Ability]
  , _triggeredAbilities     :: [Event -> Action]
  , _replacementEffects     :: [OneShotEffect -> Magic [OneShotEffect]]
  }

type Timestamp = Int

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ZoneRef = Library PlayerRef | Hand PlayerRef | Battlefield | Graveyard PlayerRef | Stack | Exile | Command
  deriving (Eq, Ord, Show, Read)

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CounterType
  = Charge | Plus1Plus1 | Minus1Minus1 | Poison | Hatchling | Loyalty
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


-- Object types

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


-- Actions

type Ability = ObjectRef -> PlayerRef -> ClosedAbility

data ClosedAbility = ClosedAbility
  { _available       :: View Bool  -- check for cost is implied
  , _manaCost        :: ManaCost
  , _additionalCosts :: [AdditionalCost]
  , _effect          :: Action
  }

data Action
  = SpecialAction  (Magic [OneShotEffect])
  | StackingAction (Magic StackItem)

type StackItem = TargetList Target (Magic [OneShotEffect])

data ManaCost = ManaCost
  { payColoredMana   :: Bag Color
  , payColorlessMana :: Int
  }

instance Monoid ManaCost where
  mempty = ManaCost [] 0
  ManaCost cs1 n1 `mappend` ManaCost cs2 n2 = ManaCost (cs1 ++ cs2) (n1 + n2)

data AdditionalCost
  = TapPermanentCost       (ObjectRef -> Bool)
  | SacrificePermanentCost (Object -> Bool)
  | ExileObjectCost       [ZoneRef] (Object -> Bool)  -- exile matching object from any of the listed zones
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
  | Flashback ManaCost
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
  = DidSimpleEffect SimpleOneShotEffect
  | DidMoveObject ZoneRef ObjectRef  -- old zone, new zone/id

  -- Keyword actions [701]
  | DidActivateAbility ObjectRef Int  -- index of ability
  | DidCastSpell PlayerRef ObjectRef  -- controller, spell
  | DidCounter ObjectRef ObjectRef  -- source (spell or ability), target
  | DidPlayLand ObjectRef
  | DidRevealCard ObjectRef
  | DidBeginStep Step
  | WillEndStep Step
  | DidLoseGame PlayerRef

data OneShotEffect
  = WillSimpleEffect SimpleOneShotEffect
  | WillMoveObject ObjectRef ZoneRef Object  -- current zone/id, new zone, suggested form

data SimpleOneShotEffect
  = AdjustLife PlayerRef Int
  | DamageObject ObjectRef ObjectRef Int Bool Bool  -- source, creature/planeswalker, amount, combat damage?, preventable?
  | DamagePlayer ObjectRef PlayerRef Int Bool Bool  -- source, player, amount, combat damage?, preventable?
  | ShuffleLibrary PlayerRef
  -- | ReorderLibraryCards
  | DrawCard PlayerRef -- Drawing is special [120.5]
  | DestroyPermanent ObjectRef Bool  -- target, preventable? -- Destruction is special [701.6b]
  | TapPermanent ObjectRef
  | UntapPermanent Id
  | AddCounter ObjectRef CounterType
  | RemoveCounter ObjectRef CounterType
  | CreateObject Object  -- create a token, emblem or spell
  | AddToManaPool PlayerRef (Maybe Color)
  | AttachPermanent ObjectRef (Maybe ObjectRef) (Maybe ObjectRef)  -- aura/equipment, old target, new target
  | RemoveFromCombat ObjectRef
  | PlayLand ObjectRef

data PriorityAction = PlayCard ObjectRef


-- Targets

data Target
  = TargetPlayer PlayerRef
  | TargetObject ObjectRef


-- Stack items

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


-- Monads

type ViewT = ReaderT World
type View = ViewT Identity

type Magic = ViewT (Operational.Program Ask)

data Ask a where
  AskKeepHand       :: PlayerRef -> Ask Bool
  AskPriorityAction :: PlayerRef -> [PriorityAction] -> Ask PriorityAction
  AskTarget         :: PlayerRef -> [Target] -> Ask Target

view :: View a -> Magic a
view v = ReaderT $ return . runIdentity . runReaderT v

$(mkLabels [''World, ''Player, ''Object, ''ObjectTypes, ''Action])

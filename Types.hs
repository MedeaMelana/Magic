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
import Control.Monad.Operational
import Data.Label (mkLabels)
import Data.Label.Pure ((:->))
import Data.IntMap (IntMap)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)


type Bag = []

type IdMap = IntMap

type PlayerRef = Id
type ObjectRef = (ZoneRef, Id)


-- | Current game situation.
data World = World
  { _players       :: IdMap Player
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
  { enterWorld :: Timestamp -> PlayerRef-> Object
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

data ZoneRef = Library PlayerRef | Hand PlayerRef | Battlefield | Graveyard PlayerRef | Stack | Exile
  deriving (Eq, Ord, Show, Read)

compileZoneRef :: ZoneRef -> World :-> IdList Object
compileZoneRef = undefined

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CounterType
  = Charge | Plus1Plus1 | Minus1Minus1 | Poison | Hatchling | Loyalty
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


-- Object types

data ObjectTypes = ObjectTypes
  { _supertypes        :: Set Supertype
  , _artifactTypes     :: Maybe (Set ArtifactType)
  , _creatureTypes     :: Maybe (Set CreatureType)
  , _enchantmentTypes  :: Maybe (Set EnchantmentType)
  , _instantTypes      :: Maybe (Set SpellType)
  , _landTypes         :: Maybe (Set LandType)
  , _planeswalkerTypes :: Maybe (Set PlaneswalkerType)
  , _sorceryTypes      :: Maybe (Set SpellType)
  } deriving (Eq, Ord, Show)

instance Monoid ObjectTypes where
  mempty = ObjectTypes mempty mempty mempty mempty mempty mempty mempty mempty
  x  `mappend` y = ObjectTypes
    { _supertypes        = _supertypes x        `mappend` _supertypes y
    , _artifactTypes     = _artifactTypes x     `mappend` _artifactTypes y
    , _creatureTypes     = _creatureTypes x     `mappend` _creatureTypes y
    , _enchantmentTypes  = _enchantmentTypes x  `mappend` _enchantmentTypes y
    , _instantTypes      = _instantTypes x      `mappend` _instantTypes y
    , _landTypes         = _landTypes x         `mappend` _landTypes y
    , _planeswalkerTypes = _planeswalkerTypes x `mappend` _planeswalkerTypes y
    , _sorceryTypes      = _sorceryTypes x      `mappend` _sorceryTypes y
    }

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

data SpellType = Arcane | Trap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data LandType = Plains | Island | Swamp | Mountain | Forest | Locus
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PlaneswalkerType = Chandra | Elspeth | Garruk | Gideon | Jace
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
  = OneShotEffectEvent OneShotEffect

  -- Keyword actions [701]
  | ActivateAbility ObjectRef Int  -- index of ability
  | CastSpell PlayerRef ObjectRef  -- controller, spell
  | Counter ObjectRef ObjectRef  -- source (spell or ability), target
  | PlayLand ObjectRef
  | RegeneratePermanent ObjectRef
  | RevealCard ObjectRef
  | ChangeStep Step Step  -- old step, new step
  | LoseGame PlayerRef

data OneShotEffect
  = AdjustLife PlayerRef Int
  | DamageObject ObjectRef ObjectRef Int Bool Bool  -- source, creature/planeswalker, amount, combat damage?, preventable?
  | DamagePlayer ObjectRef PlayerRef Int Bool Bool  -- source, player, amount, combat damage?, preventable?
  | ShuffleLibrary PlayerRef
  -- | ReorderLibraryCards
  | DrawCard PlayerRef -- Drawing is special [120.5]
  | DestroyPermanent ObjectRef Bool  -- target, preventable? -- Destruction is special [701.6b]
  | MoveObject ObjectRef ZoneRef
  | TapPermanent ObjectRef
  | UntapPermanent Id
  | AddCounter ObjectRef CounterType
  | RemoveCounter ObjectRef CounterType
  | CreateObject Object  -- create a token, emblem or spell
  | AddToManaPool PlayerRef (Maybe Color)
  | AttachPermanent ObjectRef (Maybe ObjectRef) (Maybe ObjectRef)  -- aura/equipment, old target, new target
  | RemoveFromCombat ObjectRef

data Choice
  = ChoosePlayer PlayerRef
  | ChooseObject ObjectRef
  | ChooseColor Color
  | ChooseNumber Int
  | Pass
  | Concede


-- Targets

data Target
  = TargetPlayer PlayerRef
  | TargetObject (World :-> IdList Object) ObjectRef


-- Stack items

data TargetList t a where
  Nil  :: a -> TargetList t a
  Snoc :: TargetList t (Target -> a) -> t -> TargetList t a
  Test :: (x -> a) -> (x -> Bool) -> TargetList t x -> TargetList t a

instance Functor (TargetList t) where
  fmap f (Nil x)        = Nil (f x)
  fmap f (Snoc xs t)    = Snoc (fmap (f .) xs) t
  fmap f (Test g ok xs) = Test (f . g) ok xs

instance Applicative (TargetList t) where
  pure = Nil
  xs <*> Nil b     = fmap ($ b) xs
  xs <*> Snoc ys t = Snoc ((.) <$> xs <*> ys) t
  xs <*> Test f ok ys = Test fst snd ((\g x -> (g (f x), ok x)) <$> xs <*> ys)

evaluate :: TargetList Target a -> ([Target], a)
evaluate (Nil x)       = ([], x)
evaluate (Snoc xs t)   = (ts ++ [t], f t) where (ts, f) = evaluate xs
evaluate (Test f _ xs) = (ts,        f x) where (ts, x) = evaluate xs

singleTarget :: TargetList () Target
singleTarget = Snoc (Nil id) ()

infixl 4 <?>
(<?>) :: TargetList t a -> (a -> Bool) -> TargetList t a
xs <?> ok = Test id ok xs

askTargets :: forall m a. Monad m => ([Target] -> m Target) -> [Target] -> TargetList () a -> m (TargetList Target a)
askTargets choose = askTargets' (const True)
  where
    askTargets' :: forall b. (b -> Bool) -> [Target] -> TargetList () b -> m (TargetList Target b)
    askTargets' ok ts scheme =
      case scheme of
        Nil x -> return (Nil x)
        Snoc xs () -> do
          xs' <- askTargets choose ts xs
          let (_, f) = evaluate xs'
          let eligibleTargets = filter (ok . f) ts
          chosen <- choose eligibleTargets
          return (Snoc xs' chosen)
        Test f ok' scheme' -> do
          z <- askTargets' (\x -> ok (f x) && ok' x) ts scheme'
          return (f <$> z)


-- Monads

type ViewT = ReaderT World
type View = ViewT Identity

type Magic = ViewT (Program Ask)

data Ask a where
  Ask :: PlayerRef -> [Choice] -> Ask Choice

$(mkLabels [''World, ''Player, ''Object, ''ObjectTypes, ''Action])

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Operational
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
  { _available       :: Ref Player -> View Bool  -- check for cost is implied
  , _manaCost        :: ManaCost
  , _additionalCosts :: [AdditionalCost]
  , _effect          :: Magic StackItem
  }

type StackItem = TargetList Target (Magic [OneShotEffect])

data ManaCost = ManaCost
  { payColoredMana      :: Bag Color
  , payGenericMana      :: Int
  }

data AdditionalCost
  = TapPermanentCost       (WithRef Object -> Bool)
  | SacrificePermanentCost (WithRef Object -> Bool)
  | ExileObjectCost        (WithRef Object -> Bool)
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


-- Targets

data Target
  = TargetPlayer (Ref Player)
  | TargetObject (Ref Object)


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
    askTargets' :: forall a. (a -> Bool) -> [Target] -> TargetList () a -> m (TargetList Target a)
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
  Ask :: Ref Player -> [Choice] -> Ask Choice

$(mkLabels [''World, ''Player, ''Object, ''Zone, ''Group, ''Action])

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Magic.Types (
    -- * Data structures
    Bag,

    -- * Reference types
    PlayerRef, ObjectRef, ActivatedAbilityRef, ZoneRef(..),
    LastKnownObjectInfo,

    -- * World
    World(..), players, activePlayer, activeStep, time, turnStructure, exile, battlefield, stack, command, turnHistory,

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
      pt, damage, deathtouched,
      play, staticKeywordAbilities, layeredEffects, activatedAbilities, triggeredAbilities, replacementEffects,
      temporaryEffects,

    -- * Object properties
    Timestamp, Color(..), TapStatus(..), CounterType(..), PT,

    -- * Object types
    ObjectTypes(..),
    Supertype(..), ArtifactSubtype(..), CreatureSubtype(..),
    EnchantmentSubtype(..), SpellSubtype(..), LandSubtype(..),
    PlaneswalkerSubtype(..),

    -- * Abilities
    Contextual,
    ActivatedAbility(..), TapCost(..),
    StackItem, ManaPool,
    StaticKeywordAbility(..),
    ReplacementEffect, TriggeredAbilities,
    PriorityAction(..), PayManaAction(..),

    -- * Layered effects
    LayeredEffect(..), ModifyObject(..), Layer(..),
    TemporaryLayeredEffect(..), Duration(..),

    -- * Events
    Event(..), OneShotEffect(..), SimpleOneShotEffect(..),

    -- * Targets
    Target(..), TargetList(..),

    -- * Monads @ViewT@ and @View@
    ViewT(..), View, MonadView(..),

    -- * Monadic interaction with players
    Interact(..), EventSource(..), Question(..), Pick, MonadInteract(..),

    -- * Executing effects
    ExecuteEffects(..),

    -- * Monad Magic
    Magic(..)
  ) where

import Magic.IdList (Id, IdList)

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Operational (Program, ProgramT)
import Data.Boolean
import Data.Label (mkLabels)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text, unpack)
import Prelude hiding (interact)



-- DATA STRUCTURES


type Bag = []



-- REFERENCE TYPES


type PlayerRef = Id
type ObjectRef = (ZoneRef, Id)
type ActivatedAbilityRef = (ObjectRef, Int)

data ZoneRef = Library PlayerRef | Hand PlayerRef | Battlefield | Graveyard PlayerRef | Stack | Exile | Command
  deriving (Eq, Ord, Show)

type LastKnownObjectInfo = (ObjectRef, Object)



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
  , _turnHistory   :: [Event]
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
  , _manaPool        :: ManaPool
  , _prestack        :: [(LastKnownObjectInfo, Magic ())]  -- triggered abilities about to be put on the stack, together with their source
  , _library         :: IdList Object
  , _hand            :: IdList Object
  , _graveyard       :: IdList Object
  , _maximumHandSize :: Maybe Int
  , _failedCardDraw  :: Bool  -- [704.5b]
  }



-- OBJECTS


data Card = Card
  -- timestamp, owner (and controller)
  { instantiateCard :: PlayerRef-> Object
  }

type Deck = [Card]

data Object = Object
  { _name       :: Maybe Text
  , _colors     :: Set Color
  , _types      :: ObjectTypes
  , _owner      :: PlayerRef
  , _controller :: PlayerRef
  , _timestamp  :: Timestamp

  -- for permanents on the battlefield
  , _tapStatus :: Maybe TapStatus

  -- for spells on the stack
  , _stackItem :: Maybe StackItem

  -- for creatures
  , _pt         :: Maybe PT

  -- for creatures on the battlefield
  , _damage        :: Int
  , _deathtouched  :: Bool
  --, _mustBeBlocked :: Maybe Bool
  --, _mustAttack    :: Maybe Bool

  --, _indestructible    :: Bool

  , _play                   :: Maybe ActivatedAbility
  , _staticKeywordAbilities :: Bag StaticKeywordAbility
  , _layeredEffects         :: [LayeredEffect]
  , _activatedAbilities     :: [ActivatedAbility]
  , _triggeredAbilities     :: TriggeredAbilities
  , _replacementEffects     :: [ReplacementEffect]

  -- these fields are reset whenever this object changes zones
  , _counters   :: Bag CounterType
  , _temporaryEffects       :: [TemporaryLayeredEffect]
  }

instance Show Object where
  show o =
    case _name o of
      Nothing -> "(anonymous)"
      Just n  -> unpack n



-- OBJECT PROPERTIES


type Timestamp = Int

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CounterType
  = Charge | Plus1Plus1 | Minus1Minus1 | Poison | Hatchling | Loyalty
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type PT = (Int, Int)



-- OBJECT TYPES


data ObjectTypes = ObjectTypes
  { supertypes           :: Set Supertype
  , artifactSubtypes     :: Maybe (Set ArtifactSubtype)
  , creatureSubtypes     :: Maybe (Set CreatureSubtype)
  , enchantmentSubtypes  :: Maybe (Set EnchantmentSubtype)
  , instantSubtypes      :: Maybe (Set SpellSubtype)
  , landSubtypes         :: Maybe (Set LandSubtype)
  , planeswalkerSubtypes :: Maybe (Set PlaneswalkerSubtype)
  , sorcerySubtypes      :: Maybe (Set SpellSubtype)
  } deriving (Eq, Ord, Show)

instance Monoid ObjectTypes where
  mempty = ObjectTypes mempty mempty mempty mempty mempty mempty mempty mempty
  x  `mappend` y = ObjectTypes
    { supertypes           = supertypes x           `mappend` supertypes y
    , artifactSubtypes     = artifactSubtypes x     `mappend` artifactSubtypes y
    , creatureSubtypes     = creatureSubtypes x     `mappend` creatureSubtypes y
    , enchantmentSubtypes  = enchantmentSubtypes x  `mappend` enchantmentSubtypes y
    , instantSubtypes      = instantSubtypes x      `mappend` instantSubtypes y
    , landSubtypes         = landSubtypes x         `mappend` landSubtypes y
    , planeswalkerSubtypes = planeswalkerSubtypes x `mappend` planeswalkerSubtypes y
    , sorcerySubtypes      = sorcerySubtypes x      `mappend` sorcerySubtypes y
    }

data Supertype = Basic | Legendary
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ArtifactSubtype = Equipment
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CreatureSubtype
  = Advisor | Ally | Angel | Anteater | Antelope | Ape | Archer | Archon
  | Artificer | Assassin | AssemblyWorker | Atog | Aurochs | Avatar | Badger
  | Barbarian | Basilisk | Bat | Bear | Beast | Beeble | Berserker | Bird
  | Blinkmoth | Boar | Bringer | Brushwagg | Camarid | Camel | Caribou
  | Carrier | Cat | Centaur | Cephalid | Chimera | Citizen | Cleric
  | Cockatrice | Construct | Coward | Crab | Crocodile | Cyclops | Dauthi
  | Demon | Deserter | Devil | Djinn | Dragon | Drake | Dreadnought | Drone
  | Druid | Dryad | Dwarf | Efreet | Elder | Eldrazi | Elemental | Elephant
  | Elf | Elk | Eye | Faerie | Ferret | Fish | Flagbearer | Fox | Frog
  | Fungus | Gargoyle | Germ | Giant | Gnome | Goat | Goblin | Golem | Gorgon
  | Graveborn | Gremlin | Griffin | Hag | Harpy | Hellion | Hippo | Hippogriff
  | Homarid | Homunculus | Horror | Horse | Hound | Human | Hydra | Hyena
  | Illusion | Imp | Incarnation | Insect | Jellyfish | Juggernaut | Kavu
  | Kirin | Kithkin | Knight | Kobold | Kor | Kraken | Lammasu | Leech
  | Leviathan | Lhurgoyf | Licid | Lizard | Manticore | Masticore | Mercenary
  | Merfolk | Metathran | Minion | Minotaur | Monger | Mongoose | Monk
  | Moonfolk | Mutant | Myr | Mystic | Nautilus | Nephilim | Nightmare
  | Nightstalker | Ninja | Noggle | Nomad | Octopus | Ogre | Ooze | Orb | Orc
  | Orgg | Ouphe | Ox | Oyster | Pegasus | Pentavite | Pest | Phelddagrif
  | Phoenix | Pincher | Pirate | Plant | Praetor | Prism | Rabbit | Rat
  | Rebel | Reflection | Rhino | Rigger | Rogue | Salamander | Samurai | Sand
  | Saproling | Satyr | Scarecrow | Scorpion | Scout | Serf | Serpent | Shade
  | Shaman | Shapeshifter | Sheep | Siren | Skeleton | Slith | Sliver | Slug
  | Snake | Soldier | Soltari | Spawn | Specter | Spellshaper | Sphinx | Spider
  | Spike | Spirit | Splinter | Sponge | Squid | Squirrel | Starfish | Surrakar
  | Survivor | Tetravite | Thalakos | Thopter | Thrull | Treefolk
  | Triskelavite | Troll | Turtle | Unicorn | Vampire | Vedalken | Viashino
  | Volver | Wall | Warrior | Weird | Werewolf | Whale | Wizard | Wolf
  | Wolverine | Wombat | Worm | Wraith | Wurm | Yeti | Zombie | Zuber
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


-- | Many abilities are run in the context of a source object (carrying the ability) and a player (activating or otherwise controlling it). By making this context explicit, abilities can be run in different contexts, for example by creatures \'stealing\' other creatures\' abilities.
type Contextual a = ObjectRef -> PlayerRef -> a

data ActivatedAbility = ActivatedAbility
  { available       :: Contextual (View Bool)  -- check for cost is implied
  , manaCost        :: ManaPool
  , tapCost         :: TapCost
  , effect          :: Contextual (Magic ())
  , isManaAbility   :: Bool
  }

data TapCost = NoTapCost | TapCost  -- add later: UntapCost

type StackItem = TargetList Target (ObjectRef -> Magic ())

type ManaPool = Bag (Maybe Color)

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

-- | A layered effect affects a set of objects and applies one or more
-- modifications to them. The order in which the effects are applied is
-- managed by layers [613]. By separating the affected objects from the
-- modifications, we can detect dependencies [613.7].
data LayeredEffect = LayeredEffect
  { affectedObjects :: Contextual (View [ObjectRef])
  , modifications   :: [ModifyObject]
  }

instance Show LayeredEffect where
  show _ = "(layered effect)"

-- | Temporary layered effects are created by the resolution of instants,
-- sorceries and activated abilities.
data TemporaryLayeredEffect = TemporaryLayeredEffect
  { temporaryTimestamp :: Timestamp
  , temporaryDuration  :: Duration
  , temporaryEffect    :: LayeredEffect
  }

instance Show TemporaryLayeredEffect where
  show _ = "(temporary layered effect)"

-- | Modifications of objects that are part of layered effects.
data ModifyObject
  = ChangeController PlayerRef
  | ChangeTypes (ObjectTypes -> ObjectTypes)
  | ChangeColors (Set Color -> Set Color)
  | AddStaticKeywordAbility StaticKeywordAbility
  | RemoveStaticKeywordAbility StaticKeywordAbility
  | AddActivatedAbility ActivatedAbility
  | AddTriggeredAbilities TriggeredAbilities
  | RemoveAllAbilities
  | DefinePT (View PT)
  | SetPT PT
  | ModifyPT (View PT)
  | SwitchPT

-- | Layers in which a layered effect can apply.
data Layer
  = Layer1       -- ^ Copy effects
  | Layer2       -- ^ Control-changing effects
  | Layer3       -- ^ Text-changing effects
  | Layer4       -- ^ Type-changing effects
  | Layer5       -- ^ Color-changing effects
  | Layer6       -- ^ Ability-adding and ability-removing effects
  | Layer7a      -- ^ Characteristic-defining abilities that set P/T
  | Layer7b      -- ^ Effects that set P/T
  | Layer7c      -- ^ Effects that modify P/T
  | Layer7d      -- ^ P/T counters
  | Layer7e      -- ^ Effects that switch p/t
  | LayerPlayer  -- ^ Player-affecting effects
  | LayerRules   -- ^ Rules-affecting effects
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Duration with which a 'TemporaryLayeredEffect' can apply.
data Duration
  = Indefinitely
  | UntilEndOfTurn
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type ReplacementEffect = OneShotEffect -> Maybe (Magic [OneShotEffect])

-- | Arguments: source, controller, event
type TriggeredAbilities = [Event] -> Contextual (View [Magic ()])

data PriorityAction
  = PlayCard ObjectRef
  | ActivateAbility ActivatedAbilityRef
  deriving Show

-- Actions that may be taken when paying a mana cost
data PayManaAction
  = PayManaFromManaPool (Maybe Color)
  | ActivateManaAbility ActivatedAbilityRef



-- EVENTS


-- | Events are caused by various actions in the game. They describe something that has just happened, such as executing a 'OneShotEffect', progressing to the next step or phases, casting spells, et cetera. Events form the input for triggered abilities.
data Event
  = Did SimpleOneShotEffect
  | DidMoveObject (Maybe ObjectRef) ObjectRef  -- old ref, new ref
  | DidDeclareAttackers PlayerRef [ObjectRef]

  -- Keyword actions [701]
  | DidActivateAbility ObjectRef Int  -- index of ability
  | DidCastSpell PlayerRef ObjectRef  -- controller, spell
  | DidCounter ObjectRef ObjectRef  -- source (spell or ability), target
  | DidRevealCard ObjectRef
  | DidBeginStep Step
  | WillEndStep Step
  deriving Show

-- | A one-shot effect causes a mutation in the game's state. A value of @OneShotEffect@ describes something that is about to happen. When one-shot effects are executed, they may be replaced or prevented by replacement effects, and cause an 'Event' to be raised, triggering abilities.
data OneShotEffect
  = Will SimpleOneShotEffect
  | WillMoveObject (Maybe ObjectRef) ZoneRef Object  -- optional current zone/id, new zone, suggested form
  deriving Show

-- | A one-shot effect is simple if its fields contain enough information to serve as an 'Event' unchanged, using the 'Did' constructor.
data SimpleOneShotEffect
  = GainLife PlayerRef Int
  | LoseLife PlayerRef Int
  | DamageObject Object Id Int Bool Bool  -- source, creature/planeswalker, amount, combat damage?, preventable?
  | DamagePlayer Object PlayerRef Int Bool Bool  -- source, player, amount, combat damage?, preventable?
  | ShuffleLibrary PlayerRef
  -- ReorderLibraryCards
  | DrawCard PlayerRef -- Drawing is special [120.5]
  | DestroyPermanent Id Bool  -- object on battlefield, regenerate allowed?
  | TapPermanent Id  -- object on battlefield
  | UntapPermanent Id  -- object on battlefield
  | AddCounter ObjectRef CounterType
  | RemoveCounter ObjectRef CounterType
  | AddToManaPool PlayerRef ManaPool
  | SpendFromManaPool PlayerRef ManaPool
  | AttachPermanent ObjectRef (Maybe ObjectRef) (Maybe ObjectRef)  -- aura/equipment, old target, new target
  | RemoveFromCombat Id
  | PlayLand PlayerRef ObjectRef
  | LoseGame PlayerRef
  | WinGame PlayerRef
  | InstallLayeredEffect ObjectRef TemporaryLayeredEffect
  | CeaseToExist ObjectRef
  deriving Show



-- TARGETS


data Target
  = TargetPlayer PlayerRef
  | TargetObject ObjectRef

data TargetList t a where
  Nil  :: a -> TargetList t a
  Snoc :: TargetList t (x -> a) -> (Target -> Maybe x) -> t -> TargetList t a
  Test :: (x -> a) -> (x -> View Bool) -> TargetList t x -> TargetList t a

instance Functor (TargetList t) where
  fmap f (Nil x)        = Nil (f x)
  fmap f (Snoc xs ok t) = Snoc (fmap (f .) xs) ok t
  fmap f (Test g ok xs) = Test (f . g) ok xs

instance Applicative (TargetList t) where
  pure = Nil
  xs <*> Nil b     = fmap ($ b) xs
  xs <*> Snoc ys ok t = Snoc ((.) <$> xs <*> ys) ok t
  xs <*> Test f ok ys = Test fst snd ((\g x -> (g (f x), ok x)) <$> xs <*> ys)

instance Monoid a => Monoid (TargetList t a) where
  mempty  = pure mempty
  mappend = liftA2 mappend



-- MONADS ViewT AND View


newtype ViewT m a = ViewT { runViewT :: ReaderT World m a }
  deriving (Functor, Applicative, Monad, MonadReader World, MonadTrans)

instance (Monoid a, Monad m) => Monoid (ViewT m a) where
  mempty = return mempty
  ViewT x `mappend` ViewT y = ViewT (liftM2 mappend x y)

instance (Monad m, Boolean a) => Boolean (ViewT m a) where
  true  = return true
  false = return false
  notB  = liftM  notB
  (&&*) = liftM2 (&&*)
  (||*) = liftM2 (||*)

type View = ViewT Identity

class MonadView m where
  view :: View a -> m a

instance Monad m => MonadView (ViewT m) where
  view (ViewT (ReaderT f)) = liftM (runIdentity . f) ask



-- MONADIC INTERACTION WITH PLAYERS


data Interact a where
  Debug       :: Text -> Interact ()
  LogEvents   :: EventSource -> [Event] -> World -> Interact ()
  AskQuestion :: PlayerRef -> World -> Question a -> Interact a

data EventSource
  = TurnBasedActions
    -- ^ Events caused by turn-based actions
  | StateBasedActions
    -- ^ Events caused by state-based actions
  | StackTrigger LastKnownObjectInfo
    -- ^ Events caused by putting a trigger on the stack
  | ResolutionOf Id
    -- ^ Events caused by the resolution of a spell or ability
  | PriorityActionExecution PriorityAction
    -- ^ Events caused by casting a spell or activating an ability
  deriving Show

data Question a where
  AskKeepHand              :: Question Bool
  AskPriorityAction        :: [PriorityAction] -> Question (Maybe PriorityAction)
  AskManaAbility           :: ManaPool -> [PayManaAction] -> Question PayManaAction
  AskTarget                :: [Target] -> Question Target
  AskPickReplacementEffect :: [(ReplacementEffect, Magic [OneShotEffect])] -> Question (Pick (ReplacementEffect, Magic [OneShotEffect]))
  AskPickTrigger           :: [LastKnownObjectInfo] -> Question Int

type Pick a = (a, [a])

class Monad m => MonadInteract m where
  interact :: Program Interact a -> m a



-- MONAD Magic


newtype Magic a = Magic { runMagic :: ViewT (ProgramT ExecuteEffects (Program Interact)) a }
  deriving (Functor, Applicative, Monad)

data ExecuteEffects a where
  ExecuteEffects :: [OneShotEffect] -> ExecuteEffects [Event]
  Tick           :: ExecuteEffects Timestamp

instance MonadView Magic where
  view = Magic . view

instance MonadInteract Magic where
  interact = Magic . lift . lift

instance Monoid a => Monoid (Magic a) where
  mempty  = return mempty
  mappend = liftM2 mappend


$(mkLabels [''World, ''Player, ''Object])

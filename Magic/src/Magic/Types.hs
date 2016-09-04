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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Magic.Types (
    -- * Data structures
    Bag,

    -- * Reference types
    PlayerRef, ObjectRef, SomeObjectRef, ActivatedAbilityRef, ZoneRef(..),
    ObjectType(..), LastKnownObjectInfo, toSomeObjectRef,

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
      pt, allowAttacks, allowBlocks, loyalty,
      play, alternativePlays, staticKeywordAbilities, layeredEffects, activatedAbilities, triggeredAbilities, replacementEffects,
      temporaryEffects,
    ObjectOfType(..),
      cardObject,
      permanentObject, tapStatus, damage, deathtouched, attachedTo, attacking,
      stackItemObject, stackItem,

    -- * Object properties
    Timestamp, Color(..), TapStatus(..), CounterType(..), PT,

    -- * Object types
    ObjectTypes(..),
    Supertype(..), ArtifactSubtype(..), CreatureSubtype(..),
    EnchantmentSubtype(..), SpellSubtype(..), LandSubtype(..),
    PlaneswalkerSubtype(..),

    -- * Abilities
    Contextual,
    ActivatedAbility(..), Activation(..), TapCost(..), AbilityType(..),
    StackItem, ManaPool,
    StaticKeywordAbility(..),
    ReplacementEffect, TriggeredAbilities,
    PriorityAction(..), PayManaAction(..),

    -- * Layered effects
    LayeredEffect(..), ModifyObject(..), Layer(..),
    TemporaryLayeredEffect(..), Duration(..),

    -- * Events
    Event(..), OneShotEffect(..), SimpleOneShotEffect(..), Attack(..), Block(..),

    -- * Targets
    EntityRef(..), TargetList(..),

    -- * Monads @ViewT@ and @View@
    ViewT(..), View, runView, MonadView(..),

    -- * Monadic interaction with players
    Interact(..), EventSource(..), Question(..), Pick, MonadInteract(..), Choice(..),

    -- * Executing effects
    ExecuteEffects(..),

    -- * Monad Magic
    Magic(..)
  ) where

import Magic.Some
import Magic.IdList (Id, IdList)

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Operational (Program, ProgramT)
import Data.Boolean
import Data.Label (mkLabels, lens)
import Data.Label ((:->))
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Text (Text, unpack)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import Prelude hiding (interact)



-- DATA STRUCTURES


type Bag = []



-- REFERENCE TYPES


type PlayerRef = Id
type ObjectRef ty = (ZoneRef ty, Id)
type SomeObjectRef = (Some ZoneRef, Id)
type ActivatedAbilityRef = (SomeObjectRef, Int)

toSomeObjectRef :: ObjectRef ty -> SomeObjectRef
toSomeObjectRef (zoneRef, i) = (Some zoneRef, i)

data ZoneRef :: ObjectType -> * where
  Library     :: PlayerRef -> ZoneRef TyCard
  Hand        :: PlayerRef -> ZoneRef TyCard
  Battlefield ::              ZoneRef TyPermanent
  Graveyard   :: PlayerRef -> ZoneRef TyCard
  Stack       ::              ZoneRef TyStackItem
  Exile       ::              ZoneRef TyCard
  Command     ::              ZoneRef TyCard

deriving instance Show (ZoneRef ty)
instance Show1 ZoneRef where show1 = show
deriving instance Eq (ZoneRef ty)

instance TestEquality ZoneRef where
  testEquality (Library p1)   (Library p2)   | p1 == p2 = Just Refl
  testEquality (Hand p1)      (Hand p2)      | p1 == p2 = Just Refl
  testEquality Battlefield Battlefield                  = Just Refl
  testEquality (Graveyard p1) (Graveyard p2) | p1 == p2 = Just Refl
  testEquality Stack Stack                              = Just Refl
  testEquality Exile Exile                              = Just Refl
  testEquality Command Command                          = Just Refl
  testEquality _ _ = Nothing

data ObjectType = TyCard | TyPermanent | TyStackItem

type LastKnownObjectInfo = (SomeObjectRef, Object)



-- WORLD


-- | Current game situation.
data World = World
  { _players       :: IdList Player
  , _activePlayer  :: PlayerRef
  , _activeStep    :: Step
  , _time          :: Timestamp
  , _turnStructure :: [(PlayerRef, [Step])]
  , _exile         :: IdList (ObjectOfType TyCard)
  , _battlefield   :: IdList (ObjectOfType TyPermanent)
  , _stack         :: IdList (ObjectOfType TyStackItem)
  , _command       :: IdList (ObjectOfType TyCard)
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
  , _library         :: IdList (ObjectOfType TyCard)
  , _hand            :: IdList (ObjectOfType TyCard)
  , _graveyard       :: IdList (ObjectOfType TyCard)
  , _maximumHandSize :: Maybe Int
  , _failedCardDraw  :: Bool  -- [704.5b]
  }



-- OBJECTS


data Card = Card
  -- owner (and controller)
  { instantiateCard :: PlayerRef -> Object
  }

type Deck = [Card]

data Object = Object
  { _name       :: Maybe Text
  , _colors     :: Set Color
  , _types      :: ObjectTypes
  , _owner      :: PlayerRef
  , _controller :: PlayerRef
  , _timestamp  :: Timestamp

  -- for creatures
  , _pt           :: Maybe PT
  , _allowAttacks :: [Attack] -> Contextual (View Bool)
  , _allowBlocks  :: [Block]  -> Contextual (View Bool)

  -- for planeswalkers
  , _loyalty    :: Maybe Int

  , _play                   :: Maybe Activation
  , _alternativePlays       :: [Activation]
  , _staticKeywordAbilities :: Bag StaticKeywordAbility
  , _layeredEffects         :: [LayeredEffect]
  , _activatedAbilities     :: [ActivatedAbility]
  , _triggeredAbilities     :: TriggeredAbilities
  , _replacementEffects     :: [ReplacementEffect]

  -- these fields are reset whenever this object changes zones
  , _counters               :: Bag CounterType
  , _temporaryEffects       :: [TemporaryLayeredEffect]
  }

instance Show Object where
  show o =
    case _name o of
      Nothing -> "(anonymous)"
      Just n  -> unpack n

data ObjectOfType :: ObjectType -> * where
  CardObject :: { _cardObject :: Object
                } -> ObjectOfType TyCard
  Permanent  :: { _permanentObject :: Object
                , _tapStatus       :: TapStatus
                , _damage          :: Int
                , _deathtouched    :: Bool
                , _attachedTo      :: Maybe SomeObjectRef
                , _attacking       :: Maybe EntityRef
                } -> ObjectOfType TyPermanent
  StackItem  :: { _stackItemObject :: Object
                , _stackItem       :: StackItem
                } -> ObjectOfType TyStackItem

deriving instance Show (ObjectOfType ty)


-- Some hand-written lenses because fclabels doesn't support GADTs

cardObject :: ObjectOfType TyCard :-> Object
cardObject = lens _cardObject (\f rec -> rec { _cardObject = f (_cardObject rec) })

permanentObject :: ObjectOfType TyPermanent :-> Object
permanentObject = lens _permanentObject (\f rec -> rec { _permanentObject = f (_permanentObject rec) })

tapStatus :: ObjectOfType TyPermanent :-> TapStatus
tapStatus = lens _tapStatus (\f rec -> rec { _tapStatus = f (_tapStatus rec) })

damage :: ObjectOfType TyPermanent :-> Int
damage = lens _damage (\f rec -> rec { _damage = f (_damage rec) })

deathtouched :: ObjectOfType TyPermanent :-> Bool
deathtouched = lens _deathtouched (\f rec -> rec { _deathtouched = f (_deathtouched rec) })

attachedTo :: ObjectOfType TyPermanent :-> Maybe SomeObjectRef
attachedTo = lens _attachedTo (\f rec -> rec { _attachedTo = f (_attachedTo rec) })

attacking :: ObjectOfType TyPermanent :-> Maybe EntityRef
attacking = lens _attacking (\f rec -> rec { _attacking = f (_attacking rec) })

stackItemObject :: ObjectOfType TyStackItem :-> Object
stackItemObject = lens _stackItemObject (\f rec -> rec { _stackItemObject = f (_stackItemObject rec) })

stackItem :: ObjectOfType TyStackItem :-> StackItem
stackItem = lens _stackItem (\f rec -> rec { _stackItem = f (_stackItem rec) })



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
  } deriving (Eq, Ord, Show, Read)

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
  | Wolverine | Wombat | Worm | Wraith | Wurm | Yeti | Zombie | Zubera
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
type Contextual a = SomeObjectRef -> PlayerRef -> a

data ActivatedAbility = ActivatedAbility
  { abilityActivation :: Activation
  , abilityType       :: AbilityType
  , tapCost           :: TapCost
  }

data Activation = Activation
  { timing    :: Contextual (View Bool)  -- check timing restrictions
  , available :: Contextual (View Bool)  -- check activator and current zone
  , manaCost  :: Maybe ManaPool
  , effect    :: Contextual (Magic ())
  }

data TapCost = NoTapCost | TapCost  -- add later: UntapCost

data AbilityType = ActivatedAb | ManaAb | LoyaltyAb
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type StackItem = TargetList (ObjectRef TyStackItem -> PlayerRef -> Magic ())

type ManaPool = Bag (Maybe Color)

data StaticKeywordAbility
  = Bloodthirst Int
  | Deathtouch
  | Defender
  | DoubleStrike
  | EnchantPermanent ObjectTypes
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
data LayeredEffect
  = LayeredObjectEffect
      { affectedObjects     :: Contextual (View [SomeObjectRef])
      , objectModifications :: [ModifyObject]
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
  | DefinePT (SomeObjectRef -> View PT)
  | SetPT PT
  | ModifyPT (SomeObjectRef -> View PT)
  | SwitchPT
  | RestrictAllowAttacks ([Attack] -> Contextual (View Bool))
  | RestrictAllowBlocks ([Block] -> Contextual (View Bool))

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

type ReplacementEffect =
  OneShotEffect -> Contextual (Maybe (Magic [OneShotEffect]))

type TriggeredAbilities = [Event] -> Contextual (View [Magic ()])

data PriorityAction
  = PlayCard (ObjectRef TyCard)
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
  | DidMoveObject (Maybe SomeObjectRef) SomeObjectRef  -- old ref, new ref
  | DidDeclareAttackers PlayerRef [Attack]

  -- Keyword actions [701]
  | DidActivateAbility SomeObjectRef Int  -- index of ability
  | DidCounter (ObjectRef TyStackItem) (ObjectRef TyStackItem)  -- source (spell or ability), target
  | DidBeginStep Step
  | WillEndStep Step
  deriving Show

-- | A one-shot effect causes a mutation in the game's state. A value of @OneShotEffect@ describes something that is about to happen. When one-shot effects are executed, they may be replaced or prevented by replacement effects, and cause an 'Event' to be raised, triggering abilities.
data OneShotEffect
  = Will SimpleOneShotEffect
  | forall ty. WillMoveObject (Maybe SomeObjectRef) (ZoneRef ty) (ObjectOfType ty)  -- optional current zone/id, new zone, suggested form

deriving instance Show OneShotEffect

-- | A one-shot effect is simple if its fields contain enough information to serve as an 'Event' unchanged, using the 'Did' constructor.
data SimpleOneShotEffect
  = GainLife PlayerRef Int
  | LoseLife PlayerRef Int
  | DamageObject Object (ObjectRef TyPermanent) Int Bool Bool  -- source, creature/planeswalker, amount, combat damage?, preventable?
  | DamagePlayer Object PlayerRef Int Bool Bool  -- source, player, amount, combat damage?, preventable?
  | ShuffleLibrary PlayerRef
  -- ReorderLibraryCards
  | DrawCard PlayerRef -- Drawing is special [120.5]
  | DestroyPermanent (ObjectRef TyPermanent) Bool  -- object on battlefield, regenerate allowed?
  | TapPermanent (ObjectRef TyPermanent)  -- object on battlefield
  | UntapPermanent (ObjectRef TyPermanent)  -- object on battlefield
  | AddCounter SomeObjectRef CounterType
  | RemoveCounter SomeObjectRef CounterType
  | AddToManaPool PlayerRef ManaPool
  | SpendFromManaPool PlayerRef ManaPool
  | AttachPermanent (ObjectRef TyPermanent) (Maybe SomeObjectRef) (Maybe SomeObjectRef)  -- aura/equipment, old target, new target
  | RemoveFromCombat (ObjectRef TyPermanent)
  | PlayLand PlayerRef SomeObjectRef
  | LoseGame PlayerRef
  | WinGame PlayerRef
  | InstallLayeredEffect SomeObjectRef TemporaryLayeredEffect
  | CeaseToExist SomeObjectRef
  | Sacrifice (ObjectRef TyPermanent)
  | RevealCards PlayerRef [ObjectRef TyCard]
  deriving Show

-- | A creature attacking a player or a planeswalker.
data Attack = Attack
  { -- | The creature that is attacking.
    attacker :: ObjectRef TyPermanent
    -- | The player or planeswalker being attacked.
  , attackee :: EntityRef
  } deriving Show

-- | A creature blocking another creature.
data Block = Block
  { -- | The creature blocking.
    blocker :: ObjectRef TyPermanent
    -- | The creature being blocked.
  , blockee :: ObjectRef TyPermanent
  }



-- TARGETS


data EntityRef
  = PlayerRef PlayerRef
  | ObjectRef SomeObjectRef
  deriving (Eq, Show)

data TargetList a where
  Nil  :: a -> TargetList a
  Snoc :: TargetList (x -> y)
       -> EntityRef
       -> (EntityRef -> Maybe x)
       -> (y -> View Bool)
       -> (y -> a)
       -> TargetList a

instance Functor TargetList where
  fmap f (Nil x)        = Nil (f x)
  fmap f (Snoc ts e cast test g) = Snoc ts e cast test (f . g)

instance Applicative TargetList where
  pure = Nil
  xs <*> Nil b     = fmap ($ b) xs
  xs <*> Snoc ys e cast test g =
      Snoc (f <$> xs <*> ys) e cast test' g'
    where
      f ab xy x    = (ab, xy x)
      test' (_, y) = test y
      g' (ab, y)   = ab (g y)

instance Monoid a => Monoid (TargetList a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

instance Show (TargetList a) where
  show _ = "<target list>"



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

runView :: View a -> World -> a
runView v w = runIdentity (runReaderT (runViewT v) w)

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
  | ResolutionOf (ObjectRef TyStackItem)
    -- ^ Events caused by the resolution of a spell or ability
  | PriorityActionExecution PriorityAction
    -- ^ Events caused by casting a spell or activating an ability
  deriving Show

data Choice =
    ChoiceYesNo Bool
  | ChoiceColor Color
  | ChoiceCard SomeObjectRef
  | ChoiceText Text

data Question a where
  AskKeepHand              :: Question Bool
  AskPriorityAction        :: [PriorityAction] -> Question (Maybe PriorityAction)
  AskManaAbility           :: ManaPool -> [PayManaAction] -> Question PayManaAction
  AskTarget                :: [EntityRef] -> Question EntityRef
  AskMaybeTarget           :: [EntityRef] -> Question (Maybe EntityRef)
  --AskPickReplacementEffect :: [(ReplacementEffect, Magic [OneShotEffect])] -> Question (Pick (ReplacementEffect, Magic [OneShotEffect]))
  AskPickTrigger           :: [LastKnownObjectInfo] -> Question Int
  AskAttackers             :: [ObjectRef TyPermanent] -> [EntityRef] -> Question [Attack]
  AskSearch                :: ZoneRef ty -> [Id] -> Question (Maybe Id)
  AskChoice                :: Maybe Text -> [(Choice, a)] -> Question a

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


mkLabels [''World, ''Player, ''Object]

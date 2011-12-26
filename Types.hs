{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Control.Applicative
import Control.Monad.Reader
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
  , _priority     :: Ref Player
  , _activeStep   :: Step
  , _time         :: Timestamp
  }

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

data Card = Card
  -- timestamp, owner (and controller), new ref
  { enterWorld :: Timestamp -> Ref Player -> Ref Object -> Object
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
  , _timestamp  :: Timestamp
  , _counters   :: Bag CounterType

  , _staticAbilities     :: Bag StaticAbility
  , _continuousEffects   :: [Effect]  -- special form of static ability
  , _activatedAbilities  :: [Action]
  , _triggeredAbilities  :: [Event -> Magic ()]
  }

type Timestamp = Int

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Zone = Library | Hand | Stack { _resolve :: Magic () }
  | Battlefield { _tapStatus :: TapStatus } | Graveyard | Exile

data TapStatus = Untapped | Tapped
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CounterType
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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
  { _available :: Ref Player -> View Bool  -- check for cost is implied
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
  | RemoveCounterCost (Ref Object) CounterType

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

data Effect = Effect
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

-- The 'Event' datatype exhaustively describes every single thing that can
-- happen in a game.
data Event
  -- Keyword actions [701]
  = ActivateAbility (Ref Object) Int  -- index of ability
  | Attach (Ref Object) (Ref Object)  -- aura/equipment, receiver
  | Unattach (Ref Object) (Ref Object)  -- aura/equipment, receiver
  | CastSpell (Ref Player) (Ref Object)  -- controller, spell
  | Counter (Ref Object) (Ref Object)  -- source (spell or ability), target
  | DestroyPermanent (Ref Object)
  | DiscardCard (Ref Object)
  | ExileObject (Ref Object)
  | FightCreature (Ref Object) (Ref Object)
  | PlayLand (Ref Object)
  | RegeneratePermanent (Ref Object)
  | RevealCard (Ref Object)
  | SacrificePermanent (Ref Object)
  | ShuffleLibrary (Ref Player)
  | TapObject (Ref Object)
  | UntapObject (Ref Object)
  | Search (Object -> Bool) (Object -> Bool) -- cards revealed, cards eligible

  -- Combat
  | CreatureAttacks (Ref Object)
  | CreatureBlock (RefSet Object) (RefSet Object)  -- attackers, blockers
  | CreatureDealsCombatDamage (Ref Object) (Ref Object)
  | CreatureIsDealtDamage (Ref Object) Int

  -- Misc
  | ChangeStep Step Step
  | DrawCard (Ref Player)
  | ChangeZone (Ref Object) Zone Zone
  | PutCounter (Ref Object) CounterType
  | RemoveCounter (Ref Object) CounterType
  | AdjustLife (Ref Player) Int
  | AddToManaPool (Ref Player) (Bag (Maybe Color))
  | WinGame (Ref Player)

newtype View a = View { runView :: World -> Maybe a }

instance MonadReader World View where
  ask = View Just

instance Monad View where
  return = View . const . Just

data Magic :: * -> * where
  Return       :: a -> Magic a
  Bind         :: Magic a -> (a -> Magic b) -> Magic b

  Fail         :: Magic a
  Plus         :: Magic a -> Magic a -> Magic a

  ViewWorld    :: View a -> Magic a
  PromptPlayer :: Ref Player -> [(Choice, a)] -> Magic a
  RaiseEvent   :: Event -> Magic ()

prompt :: Ref Player -> [(Choice, a)] -> Magic a
prompt = PromptPlayer

raise :: Event -> Magic ()
raise = undefined

instance Functor Magic where
  fmap = liftM

instance Applicative Magic where
  pure  = return
  (<*>) = ap

instance Monad Magic where
  return = Return
  (>>=)  = Bind

instance MonadReader World Magic where
  ask = ViewWorld ask

instance MonadPlus Magic where
  mzero = Fail
  mplus = Plus

data Choice
  = ChoosePlayer (Ref Player)
  | ChooseObject (Ref Object)
  | ChooseColor Color
  | ChooseNumber Int
  | Pass
  | Concede

$(mkLabels [''World, ''Player, ''Object, ''Zone, ''Group,
  ''PermanentType, ''Action])

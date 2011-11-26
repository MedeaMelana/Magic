module Magic where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

type Bag = []

elements :: (Bounded a, Enum a) => [a]
elements = [minBound..maxBound]


-- | Current game situation.
data Board = Board
  { players :: Bag Player
  }

data Player = Player
  { life     :: Int
  , zones    :: Map Zone (Bag Card)
  , manaPool :: Bag (Maybe Color)
  }

data Zone = Library | Hand | Graveyard | Battlefield | Stack | Exile
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


-- Card descriptions.
data Card = Card
  { name       :: Text
  , manaCost   :: Bag (Maybe Color)
  , rarity     :: Rarity
  , group      :: Group
  }

cardColors :: Card -> Set Color
cardColors card = Set.fromList (catMaybes (manaCost card))

data Rarity = Common | Uncommon | Rare | Mythic
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Color = White | Blue | Black | Red | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Group
  = Spell SpellType
  | Permanent (Set Supertype) (Set PermanentType)

data SpellType = Instant | Sorcery
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Supertype = Basic | Legendary
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PermanentType
  = Artifact
  | Creature (Set CreatureType)
  | Enchantment
  | Land
  | Planeswalker

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Magic.Description where

import Magic.Core
import Magic.Types
import Magic.IdList (Id, toList)
import Magic.Labels

import Prelude hiding (unlines)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, second)
import Control.Monad.Reader (ask)

import Data.Label.Pure (get)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..), (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Text (Text, pack)
import qualified Data.Text as Text



-- TYPE DESCRIPTION


newtype Description = Description { runDescription :: View Text }

instance IsString Description where
  fromString = Description . return . pack

instance Monoid Description where
  mempty = Description (return mempty)
  Description x `mappend` Description y = Description (mappend <$> x <*> y)

text :: Text -> Description
text = Description . return

string :: String -> Description
string = text . pack

sh :: Show a => a -> Description
sh = string . show

withWorld :: (World -> Description) -> Description
withWorld f = Description (ask >>= runDescription . f)



-- UTILITY FUNCTIONS


intercalate :: Description -> [Description] -> Description
intercalate x ys = Description (Text.intercalate <$> runDescription x <*> mapM runDescription ys)

unlines :: [Description] -> Description
--unlines xs = Description (Text.unlines <$> mapM runDescription xs)
unlines = intercalate "\n"



-- DESCRIPTIONS


describePriorityAction :: PriorityAction -> Description
describePriorityAction a =
  case a of
    PlayCard ro -> "Play " <> sh ro
    ActivateAbility ro i -> "Activate ability " <> sh i <> " of " <> sh ro

describeWorld :: Description
describeWorld = withWorld $ \world -> unlines
  [ "******************************************************************************"
  , "Player " <> sh (get activePlayer world) <> "'s turn"
  , sh (get activeStep world)
  , ""
  ] <> describeBattlefield <> describeManaPools

describeBattlefield :: Description
describeBattlefield = withWorld $ \world ->
  case toList (get battlefield world) of
    [] -> ""
    objs -> unlines ("Battlefield: " : map describeObject (objs))

describeHand :: PlayerRef -> Description
describeHand p = withWorld $ \world -> unlines $
  map describeObject (toList (get (player p .^ hand) world))

describeObject :: (Id, Object) -> Description
describeObject (i, o) = intercalate ", " components
  where
    components = [ nm, describeTypes (get types o)] ++
                    map sh (get staticKeywordAbilities o) ++ ts
    nm =
      case get name o of
        Just n -> "#" <> sh i <> " " <> text n
        Nothing -> "#" <> sh i

    ts =
      case get tapStatus o of
        Just ts' -> [sh ts']
        Nothing -> []

describeTypes :: ObjectTypes -> Description
describeTypes tys = intercalate " - " [pre, post]
  where
    pre :: Description
    pre = intercalate " " $ ls (_supertypes tys) <> map fst subtypes

    post :: Description
    post = intercalate " " (mconcat (map snd subtypes))

    mls :: (Ord a, Show a) => (ObjectTypes -> Maybe (Set a)) -> Maybe [Description]
    mls g = fmap ls (g tys)

    ls :: (Ord a, Show a) => Set a -> [Description]
    ls = map sh . sort . Set.toList

    subtypes :: [(Description, [Description])]
    subtypes = catMaybes [ subtype "Artifact"     _artifactSubtypes
                         , subtype "Creature"     _creatureSubtypes
                         , subtype "Enchantment"  _enchantmentSubtypes
                         , subtype "Instant"      _instantSubtypes
                         , subtype "Land"         _landSubtypes
                         , subtype "Planeswalker" _planeswalkerSubtypes
                         , subtype "Sorcery"      _sorcerySubtypes
                         ]

    subtype :: (Ord a, Show a) => Description -> (ObjectTypes -> Maybe (Set a)) ->
      Maybe (Description, [Description])
    subtype nm g = fmap (nm, ) (mls g)

describeManaPools :: Description
describeManaPools = withWorld $ \world -> header ("Mana pools: ") $ unlines
  [ "Player " <> sh i <> ": " <> describeManaPool (get manaPool p)
  | (i, p) <- toList (get players world)
  , not (null (get manaPool p))
  ]

describeManaPool :: Bag (Maybe Color) -> Description
describeManaPool mcs =
  case partitionMaybes (sort mcs) of
    (n, []) -> sh n
    (0, cs) -> describeColoredMana cs
    (n, cs) -> sh n <> describeColoredMana cs
  where
    describeColoredMana cs = mconcat (map (string . (: []) . head . show) cs)

partitionMaybes :: [Maybe a] -> (Int, [a])
partitionMaybes []              = (0, [])
partitionMaybes (Nothing : mxs) = first  (+ 1) (partitionMaybes mxs)
partitionMaybes (Just x : mxs)  = second (x :) (partitionMaybes mxs)

header :: Description -> Description -> Description
header (Description headerDesc) (Description bodyDesc) = Description $ do
  headerText <- headerDesc
  bodyText <- bodyDesc
  if Text.null bodyText
    then return ""
    else return (Text.unlines ["", headerText, bodyText])

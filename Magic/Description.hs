{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Magic.Description where

import Magic.Core
import Magic.Types
import Magic.IdList (Id, toList, ids)

import Prelude hiding (unlines, (.))

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, second)
import Control.Category ((.))
import Control.Monad.Reader (ask)
import Control.Monad.Reader (runReader)

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

nullDesc :: World -> Description -> Bool
nullDesc world (Description (ViewT vt)) = Text.null (runReader vt world)



-- DESCRIPTIONS


describePriorityAction :: PriorityAction -> Description
describePriorityAction a =
  case a of
    PlayCard ro@(zr, _) -> "Play from " <> describeZoneRef zr <> ": " <> describeObjectByRef ro
    ActivateAbility (ro, i) -> "Activate ability " <> sh i <> " of " <> describeObjectByRef ro

describePayManaAction :: PayManaAction -> Description
describePayManaAction a =
  case a of
    PayManaFromManaPool mc -> "Use " <> describeManaPool [mc] <> " from mana pool"
    ActivateManaAbility (ro, i) -> "Activate ability " <> sh i <> " of " <> describeObjectByRef ro

describeWorld :: Description
describeWorld = withWorld $ \world -> unlines
  [ "******************************************************************************"
  , "Player " <> sh (get activePlayer world) <> "'s turn"
  , sh (get activeStep world)
  , ""
  ] <> describePlayers <> describeZone Stack <> describeZone Battlefield <> describeManaPools

describeZone :: ZoneRef -> Description
describeZone zr = withWorld $ \world -> header (describeZoneRef zr <> ":") $
  unlines (map describeObject (toList (get (compileZoneRef zr) world)))

--describeHand :: PlayerRef -> Description
--describeHand p = withWorld $ \world -> unlines $
--  map describeObject (toList (get (player p .^ hand) world))

describeObjectByRef :: ObjectRef -> Description
describeObjectByRef ro@(_, i) = withWorld $ \world -> describeObject (i, get (object ro) world)

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

describeObjectName :: Object -> Description
describeObjectName o =
  case (get name o, get types o) of
    (Just n, _) -> text n
    (Nothing, tys) -> describeTypes tys

describeTypes :: ObjectTypes -> Description
describeTypes tys = withWorld $ \world ->
    intercalate " - " (filter (not . nullDesc world) [pre, post])
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

describeEvent :: Event -> Description
describeEvent e =
  withWorld $ \world ->
    case e of
      Did (DrawCard p) -> "Player " <> sh p <> " draws a card"
      Did (ShuffleLibrary p) -> "Player " <> sh p <> " shuffles their library"
      Did (DamagePlayer source p amount _ _) ->
        describeObjectName source <> " deals " <> sh amount <> " damage to player " <> sh p
      Did (AddToManaPool p pool) ->
        "Player " <> sh p <> " adds " <> describeManaPool pool <> " to their mana pool"
      Did (SpendFromManaPool p pool) ->
        "Player " <> sh p <> " spends " <> describeManaPool pool <> " from their mana pool"
      DidMoveObject rFromZone r@(rToZone, i) ->
        describeObjectName (get (object r) world) <> " moves from " <>
        describeZoneRef rFromZone <> " to " <> describeZoneRef rToZone
      WillEndStep s -> "End of " <> sh s
      DidBeginStep s -> "Beginning of " <> sh s
      _ -> "(event)"

describeZoneRef :: ZoneRef -> Description
describeZoneRef z =
  case z of
    Library p -> "player " <> sh p <> "'s library"
    Hand p -> "player " <> sh p <> "'s hand"
    Battlefield -> "the battlefield"
    Graveyard p -> "player " <> sh p <> "'s graveyard"
    Stack -> "the stack"
    Exile -> "exile"
    Command -> "the command zone"

describeTarget :: Target -> Description
describeTarget (TargetPlayer p) = "Player " <> sh p
describeTarget (TargetObject r) = describeObjectByRef r

describePlayers :: Description
describePlayers = withWorld $ \world ->
  unlines (map describePlayer (ids (get players world)))

describePlayer :: PlayerRef -> Description
describePlayer rPlayer = withWorld $ \world ->
  let p = get (player rPlayer) world
   in "Player " <> sh rPlayer <> ": " <> sh (get life p) <> " life"

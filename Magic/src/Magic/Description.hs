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
import Data.Monoid (Monoid(..), (<>), mempty)
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
    components = mconcat [ nm, ptd, [describeTypes (get types o)]] ++
                    map sh (get staticKeywordAbilities o) ++ ts

    nm :: [Description]
    nm = (: []) $
      case get name o of
        Just n -> "#" <> sh i <> " " <> text n
        Nothing -> "#" <> sh i <> " (anonymous)"

    ptd :: [Description]
    ptd =
      case get pt o of
        Just (p, t) -> [sh p <> "/" <> sh t]
        Nothing -> []
    ts =
      case get tapStatus o of
        Just ts' -> [sh ts']
        Nothing -> []

describeObjectNameByRef :: ObjectRef -> Description
describeObjectNameByRef ro = withWorld $ \world -> describeObjectName (get (object ro) world)

describeObjectName :: Object -> Description
describeObjectName o =
  case (get name o, get types o) of
    (Just n, _) -> text n
    (Nothing, tys) | tys /= mempty -> describeTypes tys
    _ -> "(anonymous)"

describeTypes :: ObjectTypes -> Description
describeTypes tys | tys == mempty = "(typeless)"
describeTypes tys = withWorld $ \world ->
    intercalate " - " (filter (not . nullDesc world) [pre, post])
  where
    pre :: Description
    pre = intercalate " " $ ls (supertypes tys) <> map fst subtypes

    post :: Description
    post = intercalate " " (mconcat (map snd subtypes))

    mls :: (Ord a, Show a) => (ObjectTypes -> Maybe (Set a)) -> Maybe [Description]
    mls g = fmap ls (g tys)

    ls :: (Ord a, Show a) => Set a -> [Description]
    ls = map sh . sort . Set.toList

    subtypes :: [(Description, [Description])]
    subtypes = catMaybes [ subtype "Artifact"     artifactSubtypes
                         , subtype "Creature"     creatureSubtypes
                         , subtype "Enchantment"  enchantmentSubtypes
                         , subtype "Instant"      instantSubtypes
                         , subtype "Land"         landSubtypes
                         , subtype "Planeswalker" planeswalkerSubtypes
                         , subtype "Sorcery"      sorcerySubtypes
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
      Did (DestroyPermanent i _) -> "Permanent #" <> sh i <> " is destroyed"
      Did (ShuffleLibrary p) -> "Player " <> sh p <> " shuffles their library"
      Did (DamageObject source i amount _ _) ->
        describeObjectName source <> " deals " <> sh amount <> " damage to " <> describeObjectNameByRef (Battlefield, i)
      Did (DamagePlayer source p amount _ _) ->
        describeObjectName source <> " deals " <> sh amount <> " damage to player " <> sh p
      Did (AddToManaPool p pool) ->
        "Player " <> sh p <> " adds " <> describeManaPool pool <> " to their mana pool"
      Did (SpendFromManaPool p pool) ->
        "Player " <> sh p <> " spends " <> describeManaPool pool <> " from their mana pool"
      Did (PlayLand p _) -> "Player " <> sh p <> " plays a land"
      Did (LoseGame p) -> "Player " <> sh p <> " loses"
      Did (WinGame p) -> "Player " <> sh p <> " wins the game"
      Did (CeaseToExist r) -> sh r <> " ceases to exist"
      DidMoveObject (Just (rFromZone, _)) r@(rToZone, _) ->
        describeObjectName (get (object r) world) <> " moves from " <>
        describeZoneRef rFromZone <> " to " <> describeZoneRef rToZone
      DidMoveObject Nothing r@(rToZone, _) ->
        describeObjectName (get (object r) world) <> " enters " <>
        describeZoneRef rToZone
      WillEndStep s -> "End of " <> sh s
      DidBeginStep s -> "Beginning of " <> sh s
      _ -> sh e

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Magic.Description where

import Magic
import Magic.IdList (Id, toList, ids)

import Prelude hiding (unlines, (.))

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, second)
import Control.Category ((.))
import Control.Monad.Reader (ask)
import Control.Monad.Reader (runReader)

import Data.Foldable (foldMap)
import Data.Label (get)
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
  ] <> describePlayers <> describeZone (Some Stack) <> describeZone (Some Battlefield) <> describeManaPools

describeZone :: Some ZoneRef -> Description
describeZone (Some zr) = withWorld $ \world -> header (describeZoneRef (Some zr) <> ":") $
  unlines [ describeObject i (Some o) | (i, o) <- toList (get (compileZoneRef zr) world) ]

--describeHand :: PlayerRef -> Description
--describeHand p = withWorld $ \world -> unlines $
--  map describeObject (toList (get (player p .^ hand) world))

describeObjectByRef :: SomeObjectRef -> Description
describeObjectByRef (Some z, i) = withWorld $ \world -> describeObject i (Some (get (object (z, i)) world))

describeObject :: Id -> Some ObjectOfType -> Description
describeObject i (Some typedObj) = intercalate ", " components
  where
    components = mconcat [ nm, ptd, [describeTypes (get types o)]] ++
                    map sh (get staticKeywordAbilities o) ++ ts

    o :: Object
    o = get objectPart typedObj

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

    ts :: [Description]
    ts =
      case typedObj of
        Permanent {} -> [sh (get tapStatus typedObj)]
        _ -> []

describeObjectNameByRef :: SomeObjectRef -> Description
describeObjectNameByRef ro = withWorld $ \world -> describeObjectName (get (objectBase ro) world)

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
    describeColoredMana = foldMap describeColor

describeColor :: Color -> Description
describeColor Blue = string "U"
describeColor m = string . take 1 . show $ m

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
      Did (GainLife p n) -> "Player " <> sh p <> " gains " <> sh n <> " life"
      Did (LoseLife p n) -> "Player " <> sh p <> " loses " <> sh n <> " life"
      Did (DrawCard p) -> "Player " <> sh p <> " draws a card"
      Did (DestroyPermanent i _) -> "Permanent #" <> sh i <> " is destroyed"
      Did (ShuffleLibrary p) -> "Player " <> sh p <> " shuffles their library"
      Did (DamageObject source (Battlefield, i) amount _ _) ->
        describeObjectName source <> " deals " <> sh amount <> " damage to " <> describeObjectNameByRef (Some Battlefield, i)
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
      Did (RevealCards p cs) -> unlines $ "Player " <> sh p <> " reveals" : (map (describeObjectByRef . toSomeObjectRef) cs)
      DidMoveObject (Just (rFromZone, _)) r@(rToZone, _) ->
        describeObjectName (get (objectBase r) world) <> " moves from " <>
        describeZoneRef rFromZone <> " to " <> describeZoneRef rToZone
      DidMoveObject Nothing r@(rToZone, _) ->
        describeObjectName (get (objectBase r) world) <> " enters " <>
        describeZoneRef rToZone
      WillEndStep s -> "End of " <> sh s
      DidBeginStep s -> "Beginning of " <> sh s
      _ -> sh e

describeZoneRef :: Some ZoneRef -> Description
describeZoneRef (Some z) =
  case z of
    Library p -> "player " <> sh p <> "'s library"
    Hand p -> "player " <> sh p <> "'s hand"
    Battlefield -> "the battlefield"
    Graveyard p -> "player " <> sh p <> "'s graveyard"
    Stack -> "the stack"
    Exile -> "exile"
    Command -> "the command zone"

describeEntityRef :: EntityRef -> Description
describeEntityRef (PlayerRef p) = "Player " <> sh p
describeEntityRef (ObjectRef r) = describeObjectByRef r

describePlayers :: Description
describePlayers = withWorld $ \world ->
  unlines (map describePlayer (ids (get players world)))

describePlayer :: PlayerRef -> Description
describePlayer rPlayer = withWorld $ \world ->
  let p = get (player rPlayer) world
   in "Player " <> sh rPlayer <> ": " <> sh (get life p) <> " life"

describeChoice :: Choice -> Description
describeChoice choice = case choice of
  ChoiceYesNo True -> string "Yes"
  ChoiceYesNo False -> string "No"

  ChoiceColor color -> sh color

  ChoiceCard someRef -> describeObjectByRef someRef

  ChoiceText txt -> text txt

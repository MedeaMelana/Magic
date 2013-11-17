{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Magic.Json (interactToJSON) where

import Magic
import Magic.Engine.Types
import qualified Magic.IdList as IdList

import Control.Monad (liftM)

import Data.Aeson (ToJSON(..), Value(..), (.=))
import Data.Aeson.Types (Pair)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text

import Safe (atMay, readMay)



obj :: [Pair] -> Value
obj = Aeson.object

typedObject :: (Text, [Pair]) -> Value
typedObject (typeName, props) = obj ("type" .= String typeName : props)

class ToJSONPairs a where
  toJSONPairs :: a -> [Pair]



instance ToJSON IdList.Id where
  toJSON = toJSON . IdList.idToInt

instance ToJSON a => ToJSON (IdList.IdList a) where
  toJSON list = obj ( "order" .= IdList.ids list
                    : map (\(i,x) -> Text.pack (show i) .= toJSON x) (IdList.toList list)
                    )



someObjectRefToJSON :: SomeObjectRef -> Value
someObjectRefToJSON (Some z, i) = obj [ "zone" .= z, "objectId" .= i ]

objectRefToJSON :: ObjectRef ty -> Value
objectRefToJSON (z, i) = obj [ "zone" .= z, "objectId" .= i ]

activatedAbilityRefToJSON :: ActivatedAbilityRef -> Value
activatedAbilityRefToJSON (r, i) =
  obj [ "objectRef" .= someObjectRefToJSON r, "index" .= i ]

instance ToJSON (ZoneRef ty) where
  toJSON z = case z of
    Library p   -> obj [ "name" .= String "library", "playerId" .= p ]
    Hand p      -> obj [ "name" .= String "hand", "playerId" .= p ]
    Battlefield -> obj [ "name" .= String "battlefield" ]
    Graveyard p -> obj [ "name" .= String "graveyard", "playerId" .= p ]
    Stack       -> obj [ "name" .= String "stack" ]
    Exile       -> obj [ "name" .= String "exile" ]
    Command     -> obj [ "name" .= String "command" ]

someZoneRefToJSON :: Some ZoneRef -> Value
someZoneRefToJSON (Some z) = toJSON z

lkiToJSON :: LastKnownObjectInfo -> Value
lkiToJSON (r, o) = obj [ "objectRef" .= someObjectRefToJSON r, "object" .= obj (toJSONPairs o) ]


instance ToJSON World where
  toJSON w = obj
    [ "players"        .= _players w
    , "activePlayerId" .= _activePlayer w
    , "activeStep"     .= _activeStep w
    , "time"           .= _time w
    , "exile"          .= _exile w
    , "battlefield"    .= _battlefield w
    , "stack"          .= _stack w
    , "command"        .= _command w
    ]



instance ToJSON Step where
  toJSON phase = obj $ case phase of
    BeginningPhase step -> ("phase" .= String "beginning") : toJSONPairs step
    MainPhase ->           ("phase" .= String "main")      : []
    CombatPhase step ->    ("phase" .= String "combat")    : toJSONPairs step
    EndPhase step ->       ("phase" .= String "end")       : toJSONPairs step

instance ToJSONPairs BeginningStep where
  toJSONPairs step = ["step" .= String value]
    where
      value = case step of
        UntapStep  -> "untap"
        UpkeepStep -> "upkeep"
        DrawStep   -> "draw"

instance ToJSONPairs CombatStep where
  toJSONPairs step = ["step" .= String value]
    where
      value = case step of
        BeginningOfCombatStep -> "beginningOfCombat"
        DeclareAttackersStep  -> "declareAttackers"
        DeclareBlockersStep   -> "declareBlockers"
        CombatDamageStep      -> "combatDamage"
        EndOfCombatStep       -> "endOfCombat"

instance ToJSONPairs EndStep where
  toJSONPairs step = ["step" .= String value]
    where
      value = case step of
        EndOfTurnStep -> "endOfTurn"
        CleanupStep   -> "cleanup"



instance ToJSON Player where
  toJSON p = obj
    [ "life"      .= _life p
    , "manaPool"  .= _manaPool p
    , "library"   .= _library p
    , "hand"      .= _hand p
    , "graveyard" .= _graveyard p
    ]



instance ToJSON (ObjectOfType a) where
  toJSON (CardObject o) = obj (toJSONPairs o)
  toJSON p@(Permanent {}) =
      obj (toJSONPairs (_permanentObject p) ++ permProps)
    where
      permProps = [ "tapStatus" .= _tapStatus p, "damage"    .= _damage p ]
  toJSON (StackItem o _) = obj (toJSONPairs o)

instance ToJSONPairs Object where
  toJSONPairs o =
    [ "name" .= _name o
    , "colors" .= _colors o
    , "types" .= _types o
    , "ownerId" .= _owner o
    , "controllerId" .= _controller o
    , "timestamp" .= _timestamp o
    , "counters" .= _counters o
    , "pt" .= maybe Null (\(p,t) -> obj [ "power" .= p, "toughness" .= t ]) (_pt o)
    , "staticKeywordAbilities" .= _staticKeywordAbilities o
    ]



instance ToJSON Color where
  toJSON = toJSON . map toLower . show

instance ToJSON TapStatus where
  toJSON = toJSON . map toLower . show

instance ToJSON CounterType where
  toJSON = toJSON . map toLower . show



instance ToJSON ObjectTypes where
  toJSON tys = obj
    [ "supertypes"        .= supertypes tys
    , "artifactTypes"     .= artifactSubtypes tys
    , "creatureTypes"     .= creatureSubtypes tys
    , "enchantmentTypes"  .= enchantmentSubtypes tys
    , "instantTypes"      .= instantSubtypes tys
    , "landTypes"         .= landSubtypes tys
    , "planeswalkerTypes" .= planeswalkerSubtypes tys
    ]

instance ToJSON Supertype           where toJSON = toJSON . show
instance ToJSON ArtifactSubtype     where toJSON = toJSON . show
instance ToJSON CreatureSubtype     where toJSON = toJSON . show
instance ToJSON EnchantmentSubtype  where toJSON = toJSON . show
instance ToJSON SpellSubtype        where toJSON = toJSON . show
instance ToJSON LandSubtype         where toJSON = toJSON . show
instance ToJSON PlaneswalkerSubtype where toJSON = toJSON . show


instance ToJSON StaticKeywordAbility where
  toJSON = \case
    FirstStrike -> "first strike"
    ProtectionFromColor c ->
      toJSON ("protection from " ++ map toLower (show c))
    ab -> (toJSON . map toLower . show) ab

instance ToJSON PriorityAction where
  toJSON a = typedObject $ case a of
    PlayCard r        -> ("playCard", [ "objectRef" .= someObjectRefToJSON r ])
    ActivateAbility r -> ("activateAbility", [ "activatedAbilityRef" .= activatedAbilityRefToJSON r ])

instance ToJSON PayManaAction where
  toJSON a = typedObject $ case a of
    PayManaFromManaPool mc ->
      ("payManaFromManaPool", [ "color" .= mc ])
    ActivateManaAbility r ->
      ("activateManaAbility", [ "activatedAbillityRef" .= activatedAbilityRefToJSON r])




instance ToJSON Event where
  toJSON event = typedObject $ case event of
    Did (GainLife p n)         -> ("gainLife",
      ["playerId" .= p, "amount" .= n])
    Did (LoseLife p n)         -> ("loseLife",
      ["playerId" .= p, "amount" .= n])
    Did (DamageObject _ i n c p) -> ("damageObject",
      [ "objectId" .= i, "amount" .= n, "isCombatDamage" .= c ])
    Did (DamagePlayer _ r n c p) -> ("damagePlayer",
      [ "playerId" .= r, "amount" .= n, "isCombatDamage" .= c ])
    Did (ShuffleLibrary p)       -> ("shuffleLibrary", [ "playerId" .= p ])
    Did (DrawCard p)             -> ("drawCard", [ "playerId" .= p ])
    Did (DestroyPermanent i _)   -> ("destroyPermanent", [ "objectId" .= i ])
    Did (TapPermanent i)         -> ("tapPermenent", [ "objectId" .= i ])
    Did (UntapPermanent i)       -> ("untapPermenent", [ "objectId" .= i ])
    Did (AddToManaPool p m)      -> ("addToManaPool",
      [ "playerId" .= p, "mana" .= m ])
    Did (SpendFromManaPool p m)  -> ("spendFromManaPool",
      [ "playerId" .= p, "mana" .= m ])
    Did (PlayLand p r)           -> ("playLand",
      [ "playerId" .= p, "objectRef" .= someObjectRefToJSON r ])
    Did (LoseGame p)             -> ("loseGame", [ "playerId" .= p ])
    Did (WinGame p)              -> ("winGame", [ "playerId" .= p ])
    Did (CeaseToExist r)         -> ("ceaseToExist",
      [ "objectRef" .= someObjectRefToJSON r ])
    DidMoveObject mOldRef newRef -> ("moveObject",
      [ "oldRef" .= maybe Null someObjectRefToJSON mOldRef
      , "newRef" .= someObjectRefToJSON newRef ])
    DidBeginStep step            -> ("beginStep", [ "step" .= step ])
    WillEndStep step             -> ("willEndStep", [ "step" .= step ])

instance ToJSON GameOver where
  toJSON t = typedObject $ case t of
    GameWin p -> ("gameWin", [ "playerId" .= p ])
    GameDraw -> ("gameDraw", [])
    ErrorWithMessage msg -> ("error", [ "message" .= msg ])
    UnknownError -> ("error", [])

instance ToJSON EntityRef where
  toJSON t = typedObject $ case t of
    PlayerRef p -> ("player", [ "playerId" .= p ])
    ObjectRef r -> ("object", [ "objectRef" .= someObjectRefToJSON r ])

interactToJSON :: Monad m => m Text -> Interact a -> (Value, m a)
interactToJSON receiveData op = (typedObject (instrType, props), receiveAnswer)
  where
    (instrType, props, receiveAnswer) = case op of
      Debug msg -> ("debug", [ "message" .= msg ], return ())
      LogEvents _source events world ->
        ("logEvents", [ "events" .= events, "world" .= world ], return ())
      AskQuestion p w q ->
        let (json, select) = questionToJSON q
            getAnswer = do
              input <- Text.unpack `liftM` receiveData
              case readMay input >>= select of
                Nothing -> do
                  --sendText ("Invalid option" :: Text)
                  getAnswer
                Just x ->
                  return x
          in ("askQuestion", [ "playerId" .= p, "world" .= w, "question" .= json], getAnswer)

questionToJSON :: Question a -> (Value, Int -> Maybe a)
questionToJSON q = (typedObject (questionType, props), select)
  where
    (questionType, props, select) = case q of
      AskKeepHand ->
        ("keepHand", [ "options" .= [True, False] ], atMay [True, False])
      AskPriorityAction opts ->
        ("priorityAction", [ "options" .= (passOption : map toJSON opts) ],
          \i -> case i of 0 -> Just Nothing; _ -> fmap Just (atMay opts (i - 1)))
      AskManaAbility m opts ->
        ("manaAbility", [ "manaToPay" .= m, "options" .= opts ], atMay opts)
      AskTarget ts ->
        ("target", ["options" .= ts], atMay ts)
      AskPickTrigger lkis ->
        ("pickTrigger", ["options" .= map lkiToJSON lkis],
          \i -> if i < length lkis then Just i else Nothing)

passOption :: Value
passOption = toJSON (typedObject ("pass", []))

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Magic.Json (interactToJSON) where

import Magic
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



objectRefToJSON :: ObjectRef -> Value
objectRefToJSON (z, i) = obj [ "zone" .= z, "objectId" .= i ]

activatedAbilityRefToJSON :: ActivatedAbilityRef -> Value
activatedAbilityRefToJSON (r, i) =
  obj [ "objectRef" .= objectRefToJSON r, "index" .= i ]

instance ToJSON ZoneRef where
  toJSON z = case z of
    Library p   -> obj [ "name" .= String "library", "playerId" .= p ]
    Hand p      -> obj [ "name" .= String "hand", "playerId" .= p ]
    Battlefield -> obj [ "name" .= String "battlefield" ]
    Graveyard p -> obj [ "name" .= String "graveyard", "playerId" .= p ]
    Stack       -> obj [ "name" .= String "stack" ]
    Exile       -> obj [ "name" .= String "exile" ]
    Command     -> obj [ "name" .= String "command" ]

lkiToJSON :: LastKnownObjectInfo -> Value
lkiToJSON (r, o) = obj [ "objectRef" .= objectRefToJSON r, "object" .= o ]


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



instance ToJSON Object where
  toJSON o = obj
    [ "name" .= _name o
    , "colors" .= _colors o
    --, "types"
    , "ownerId" .= _owner o
    , "controllerId" .= _controller o
    , "timestamp" .= _timestamp o
    , "counters" .= _counters o
    , "tapStatus" .= _tapStatus o
    , "pt" .= maybe Null (\(p,t) -> obj [ "power" .= p, "toughness" .= t ]) (_pt o)
    , "damage" .= _damage o
    --, "staticKeywordAbilities" .= _staticKeywordAbilities
    ]



instance ToJSON Color where
  toJSON = toJSON . map toLower . show

instance ToJSON TapStatus where
  toJSON = toJSON . map toLower . show

instance ToJSON CounterType where
  toJSON = toJSON . map toLower . show



instance ToJSON PriorityAction where
  toJSON a = typedObject $ case a of
    PlayCard r        -> ("playCard", [ "objectRef" .= objectRefToJSON r ])
    ActivateAbility r -> ("activateAbility", [ "activatedAbillityRef" .= activatedAbilityRefToJSON r ])

instance ToJSON PayManaAction where
  toJSON a = typedObject $ case a of
    PayManaFromManaPool mc ->
      ("payManaFromManaPool", [ "color" .= mc ])
    ActivateManaAbility r ->
      ("activateManaAbility", [ "activatedAbillityRef" .= r])




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
      [ "playerId" .= p, "objectRef" .= objectRefToJSON r ])
    Did (LoseGame p)             -> ("loseGame", [ "playerId" .= p ])
    Did (WinGame p)              -> ("winGame", [ "playerId" .= p ])
    Did (CeaseToExist r)         -> ("ceaseToExist",
      [ "objectRef" .= objectRefToJSON r ])
    DidMoveObject mOldRef newRef -> ("moveObject",
      [ "oldRef" .= maybe Null objectRefToJSON mOldRef
      , "newRef" .= objectRefToJSON newRef ])
    DidBeginStep step            -> ("beginStep", [ "step" .= step ])
    WillEndStep step             -> ("willEndStep", [ "step" .= step ])



instance ToJSON Target where
  toJSON t = typedObject $ case t of
    TargetPlayer p -> ("player", [ "playerId" .= p ])
    TargetObject r -> ("object", [ "objectRef" .= r ])

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

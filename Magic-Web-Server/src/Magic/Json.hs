{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Magic.Json (ToJSONPairs(..)) where

import Magic

import Data.Aeson (ToJSON(..), Value(..), (.=))
import Data.Aeson.Types (Pair)
import qualified Data.Aeson as Aeson

instance ToJSON (Interact a) where
  toJSON op = case op of
    Debug msg -> obj [ "type" .= String "debug", "message" .= msg ]
    LogEvents _source events _world ->
      obj [ "type" .= String "logEvents", "events" .= events ]

instance ToJSON Event where
  toJSON event = case event of
    DidBeginStep step -> obj [ "type" .= String "beginStep", "step" .= step ]
    WillEndStep step  -> obj [ "type" .= String "willEndStep", "step" .= step ]

class ToJSONPairs a where
  toJSONPairs :: a -> [Pair]

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
        UntapStep -> "untap"
        UpkeepStep -> "upkeep"
        DrawStep -> "draw"

instance ToJSONPairs CombatStep where
  toJSONPairs step = ["step" .= String value]
    where
      value = case step of
        BeginningOfCombatStep -> "beginningOfCombat"
        DeclareAttackersStep -> "declareAttackers"
        DeclareBlockersStep -> "declareBlockers"
        CombatDamageStep -> "combatDamage"
        EndOfCombatStep -> "endOfCombat"

instance ToJSONPairs EndStep where
  toJSONPairs step = ["step" .= String value]
    where
      value = case step of
        EndOfTurnStep -> "endOfTurn"
        CleanupStep -> "cleanup"

obj :: [Pair] -> Value
obj = Aeson.object

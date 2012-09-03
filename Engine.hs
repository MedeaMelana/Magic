{-# LANGUAGE TypeOperators #-}

module Engine where

import qualified IdList
import Labels
import Predicates
import Types

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Control.Monad.Operational
import Control.Monad.Random (RandT, StdGen)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Ord (comparing)
import Data.Label.Pure (set)
import Data.Label.PureM (gets, puts, (=:))
import Data.List (sortBy)
import Data.Traversable (for)

type Engine = StateT World (RandT StdGen (Program Ask))

round :: Engine ()
round = forever $ do
  players ~:* set manaPool []
  nextStep >>= executeStep

nextStep :: Engine Step
nextStep = do
  (rp, s : ss) : ts <- gets turnStructure
  turnStructure =: if null ss then ts else (rp, ss) : ts
  activePlayer  =: rp
  activeStep    =: s
  return s

executeStep :: Step -> Engine ()

executeStep (BeginningPhase UntapStep) = do
  -- TODO [502.1]  phasing

  -- [502.2] untap permanents
  rp <- gets activePlayer
  ios <- IdList.filter (isControlledBy rp) <$> gets battlefield
  _ <- for ios $ \(i, _) -> executeEffect (UntapPermanent i)
  return ()

executeStep (BeginningPhase UpkeepStep) = do
  -- TODO [503.1]  handle triggers

  -- [503.2]
  offerPriority

executeStep (BeginningPhase DrawStep) = do
  -- [504.1]
  DrawCard <$> gets activePlayer >>= executeEffect

  -- TODO [504.2]  handle triggers

  -- [504.3]
  offerPriority

executeStep MainPhase = do
  -- TODO [505.4]  handle triggers

  -- [505.5]
  offerPriority

executeStep (CombatPhase BeginningOfCombatStep) = do
  offerPriority

executeStep (CombatPhase DeclareAttackersStep) = do
  -- TODO [508.1a] declare attackers
  -- TODO [508.1b] declare which player or planeswalker each attacker attacks
  -- TODO [508.1c] check attacking restrictions
  -- TODO [508.1d] check attacking requirements
  -- TODO [508.1e] declare banding
  -- TODO [508.1f] tap attackers
  -- TODO [508.1g] determine costs
  -- TODO [508.1h] allow mana abilities
  -- TODO [508.1i] pay costs
  -- TODO [508.1j] mark creatures as attacking
  -- TODO [508.2]  handle triggers
  offerPriority
  -- TODO [508.6]  potentially skip declare blockers and combat damage steps
  return ()

executeStep (CombatPhase DeclareBlockersStep) = do
  -- TODO [509.1a] declare blockers
  -- TODO [509.1b] check blocking restrictions
  -- TODO [509.1c] check blocking requirements
  -- TODO [509.1d] determine costs
  -- TODO [509.1e] allow mana abilities
  -- TODO [509.1f] pay costs
  -- TODO [509.1g] mark creatures as blocking
  -- TODO [509.1h] mark creatures as blocked
  -- TODO [509.2]  declare attackers' damage assignment order
  -- TODO [509.3]  declare blockers' damage assignment order
  -- TODO [509.4]  handle triggers
  offerPriority
  -- TODO [509.6]  determine new attackers' damage assignment order
  -- TODO [509.7]  determine new blockers' damage assignment order
  return ()

executeStep (CombatPhase CombatDamageStep) = do
  -- TODO [510.1]  assign combat damage
  -- TODO [510.2]  deal damage
  -- TODO [510.3]  handle triggers
  offerPriority
  -- TODO [510.5]  possibly introduce extra combat damage step for first/double strike
  return ()

executeStep (CombatPhase EndOfCombatStep) = do
  -- TODO [511.1]  handle triggers

  -- [511.2]
  offerPriority

  -- TODO [511.3]  remove creatures from combat
  return ()

executeStep (EndPhase EndOfTurnStep) = do
  -- TODO [513.1]  handle triggers
  
  -- [513.2]
  offerPriority

executeStep (EndPhase CleanupStep) = do
  -- TODO [514.1]  discard excess cards
  -- TODO [514.2]  remove damage from permanents
  -- TODO [514.3]  handle triggers; check state-based actions; possibly offer priority
  return ()

executeEffect :: OneShotEffect -> Engine ()
executeEffect e = do
  -- TODO trigger abilities
  -- TODO apply replacement effects
  compileEffect e

compileEffect :: OneShotEffect -> Engine ()
compileEffect (UntapPermanent ro) =
  battlefield .^ listEl ro .^ tapStatus =: Just Untapped
compileEffect (DrawCard rp) = do
  lib <- gets (players .^ listEl rp .^ library)
  case IdList.toList lib of
    []          -> players .^ listEl rp .^ failedCardDraw =: True
    (ro, _) : _ -> executeEffect (MoveObject (Library rp, ro) (Hand rp))
compileEffect (MoveObject rObject@(rFromZone, i) rToZone) = do
  mObject <- lookupObject rObject
  case mObject of
    Nothing -> return ()
    Just object -> do
      tick >>= puts (compileZoneRef rToZone .^ listEl i .^ timestamp)
      compileZoneRef rFromZone ~: IdList.remove i
      compileZoneRef rToZone   ~: IdList.cons object
compileEffect (ShuffleLibrary rPlayer) = do
  let libraryLabel = players .^ listEl rPlayer .^ library
  lib <- gets libraryLabel
  lib' <- lift (IdList.shuffle lib)
  puts libraryLabel lib'
compileEffect _ = undefined

tick :: Engine Timestamp
tick = do
  t <- gets time
  time ~: succ
  return t

lookupObject :: ObjectRef -> Engine (Maybe Object)
lookupObject (rz, i) = IdList.get i <$> gets (compileZoneRef rz)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

offerPriority :: Engine ()
offerPriority = do
  -- TODO check state-based actions
  -- TODO empty prestacks in APNAP order
  -- TODO offer available actions to players in APNAP order
  -- TODO when everyone passes, return
  return ()

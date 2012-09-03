{-# LANGUAGE TypeOperators #-}

module Engine where

import IdList (Id)
import qualified IdList
import Labels
import Predicates
import Types
import Utils

import Control.Applicative ((<$>))
import Control.Monad (forever, void, forM_, replicateM_)
import qualified Control.Monad.Operational as Operational
import Control.Monad.Random (RandT, StdGen)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Ord (comparing)
import Data.Label.Pure (set)
import Data.Label.PureM (gets, puts, (=:))
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

type Engine = StateT World (RandT StdGen (Operational.Program Ask))


enterPlayer :: [Card] -> Engine ()
enterPlayer deck = do
  playerId <- IdList.consM players player
  forM_ deck $ \card -> do
    t <- tick
    IdList.consM (players .^ listEl playerId .^ library) (instantiateCard card t playerId)

drawOpeningHands :: [PlayerRef] -> Int -> Engine ()
drawOpeningHands [] _ =
  return ()
drawOpeningHands playerIds 0 =
  forM_ playerIds shuffleLibrary
drawOpeningHands playerIds handSize = do
  mulliganingPlayers <-
    for playerIds $ \playerId -> do
      moveAllObjects (Hand playerId) (Library playerId)
      shuffleLibrary playerId
      replicateM_ handSize (drawCard playerId)
      keepHand <- liftQuestion (AskKeepHand playerId)
      if keepHand
        then return Nothing
        else return (Just playerId)
  drawOpeningHands (catMaybes mulliganingPlayers) (handSize - 1)

liftQuestion :: Ask a -> Engine a
liftQuestion = lift . lift . Operational.singleton

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


-- Execution of steps

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


-- Compilation of effects

compileEffect :: OneShotEffect -> Engine ()
compileEffect (UntapPermanent i) = untapPermanent i
compileEffect (DrawCard rp) = drawCard rp
compileEffect (MoveObject rObj rToZone) = moveObject rObj rToZone
compileEffect (ShuffleLibrary rPlayer) = shuffleLibrary rPlayer
compileEffect _ = undefined


tick :: Engine Timestamp
tick = do
  t <- gets time
  time ~: succ
  return t

untapPermanent :: Id -> Engine ()
untapPermanent ro = battlefield .^ listEl ro .^ tapStatus =: Just Untapped

drawCard :: PlayerRef -> Engine ()
drawCard rp = do
  lib <- gets (players .^ listEl rp .^ library)
  case IdList.toList lib of
    []          -> players .^ listEl rp .^ failedCardDraw =: True
    (ro, _) : _ -> executeEffect (MoveObject (Library rp, ro) (Hand rp))

moveObject :: ObjectRef -> ZoneRef -> Engine ()
moveObject (rFromZone, i) rToZone = do
  mObj <- IdList.removeM (compileZoneRef rFromZone) i
  case mObj of
    Nothing     -> return ()
    Just obj -> do
      t <- tick
      void (IdList.consM (compileZoneRef rToZone) (set timestamp t obj))

moveAllObjects :: ZoneRef -> ZoneRef -> Engine ()
moveAllObjects rFromZone rToZone = do
  objectIds <- map fst . IdList.toList <$> gets (compileZoneRef rFromZone)
  forM_ objectIds $ \i -> moveObject (rFromZone, i) rToZone

shuffleLibrary :: PlayerRef -> Engine ()
shuffleLibrary rPlayer = do
  let libraryLabel = players .^ listEl rPlayer .^ library
  lib <- gets libraryLabel
  lib' <- lift (IdList.shuffle lib)
  puts libraryLabel lib'

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

offerPriority :: Engine ()
offerPriority = do
  -- TODO check state-based actions
  -- TODO empty prestacks in APNAP order
  -- TODO offer available actions to players in APNAP order
  -- TODO when everyone passes, return
  return ()

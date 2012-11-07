{-# LANGUAGE TypeOperators #-}

module Engine where

import Core
import IdList (Id)
import qualified IdList
import Labels
import Predicates
import Utils hiding (object)

import Control.Applicative ((<$>))
import Control.Monad (forever, forM_, replicateM_, when)
import qualified Control.Monad.Operational as Operational
import Control.Monad.Random (RandT, StdGen)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell, execWriterT)
import Data.Either (partitionEithers)
import Data.Ord (comparing)
import Data.Label.Pure (get, set, (:->))
import Data.Label.PureM (gets, puts, (=:))
import Data.List (sortBy)
import Data.Maybe (catMaybes, isJust)
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
  step <- nextStep
  raise (DidBeginStep step)
  executeStep step
  raise (WillEndStep step)

nextStep :: Engine Step
nextStep = do
  (rp, s : ss) : ts <- gets turnStructure
  turnStructure =: if null ss then ts else (rp, ss) : ts
  activePlayer  =: rp
  activeStep    =: s
  return s

raise :: Event -> Engine ()
raise _ = do
  -- TODO handle triggered abilities
  return ()


-- Execution of steps

executeStep :: Step -> Engine ()

executeStep (BeginningPhase UntapStep) = do
  -- TODO [502.1]  phasing

  -- [502.2] untap permanents
  rp <- gets activePlayer
  ios <- IdList.filter (isControlledBy rp) <$> gets battlefield
  _ <- for ios $ \(i, _) -> executeEffect (Will (UntapPermanent i))
  return ()

executeStep (BeginningPhase UpkeepStep) = do
  -- TODO [503.1]  handle triggers

  -- [503.2]
  offerPriority

executeStep (BeginningPhase DrawStep) = do
  -- [504.1]
  ap <- gets activePlayer
  executeEffect (Will (DrawCard ap))

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

-- | Execute a one-shot effect, applying replacement effects and triggering abilities.
executeEffect :: OneShotEffect -> Engine ()
executeEffect e = applyReplacementEffects e >>= mapM_ compileEffect


-- Compilation of effects

compileEffect :: OneShotEffect -> Engine ()
compileEffect e =
  case e of
    WillMoveObject rObj rToZone obj -> moveObject rObj rToZone obj
    Will (UntapPermanent i)         -> untapPermanent i
    Will (DrawCard rp)              -> drawCard rp
    Will (ShuffleLibrary rPlayer)   -> shuffleLibrary rPlayer
    _ -> undefined



tick :: Engine Timestamp
tick = do
  t <- gets time
  time ~: succ
  return t

untapPermanent :: Id -> Engine ()
untapPermanent i = do
  Just ts <- gets (battlefield .^ listEl i .^ tapStatus)
  case ts of
    Untapped -> return ()
    Tapped -> do
      battlefield .^ listEl i .^ tapStatus =: Just Untapped
      raise (Did (UntapPermanent i))

drawCard :: PlayerRef -> Engine ()
drawCard rp = do
  lib <- gets (players .^ listEl rp .^ library)
  case IdList.toList lib of
    []          -> do
      players .^ listEl rp .^ failedCardDraw =: True
    (ro, o) : _ -> do
      executeEffect (WillMoveObject (Library rp, ro) (Hand rp) o)
      -- TODO Only raise event if card was actually moved
      raise (Did (DrawCard rp))

moveObject :: ObjectRef -> ZoneRef -> Object -> Engine ()
moveObject (rFromZone, i) rToZone obj = do
  mObj <- IdList.removeM (compileZoneRef rFromZone) i
  case mObj of
    Nothing -> return ()
    Just _  -> do
      t <- tick
      newId <- IdList.consM (compileZoneRef rToZone) (set timestamp t obj)
      raise (DidMoveObject rFromZone (rToZone, newId))

moveAllObjects :: ZoneRef -> ZoneRef -> Engine ()
moveAllObjects rFromZone rToZone = do
  ois <- IdList.toList <$> gets (compileZoneRef rFromZone)
  forM_ ois $ \(i, o) -> moveObject (rFromZone, i) rToZone o

shuffleLibrary :: PlayerRef -> Engine ()
shuffleLibrary rPlayer = do
  let libraryLabel = players .^ listEl rPlayer .^ library
  lib <- gets libraryLabel
  lib' <- lift (IdList.shuffle lib)
  puts libraryLabel lib'
  raise (Did (ShuffleLibrary rPlayer))

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

offerPriority :: Engine ()
offerPriority = do
    -- TODO do this in a loop
    checkSBAs
    processPrestacks
    mAction <- apnap >>= offerPriority'
    case mAction of
      Just action -> do
        -- TODO execute actions
        offerPriority
      Nothing -> do
        st <- gets stack
        case IdList.head st of
          Nothing -> return ()
          Just (i, _) -> do
            resolve i
            offerPriority
  where
    offerPriority' ((p, _):ps) = do
      actions <- collectActions p
      mAction <- liftQuestion (AskPriorityAction p actions)
      case mAction of
        Just action -> return (Just action)
        Nothing -> offerPriority' ps
    offerPriority' [] = return Nothing

checkSBAs :: Engine ()
checkSBAs = do
  sbas <- collectSBAs
  sbas' <- concat <$> for sbas applyReplacementEffects
  forM_ sbas' executeEffect

collectSBAs :: Engine [OneShotEffect]
collectSBAs = execWriterT $ do
    checkPlayers
    checkBattlefield
    -- TODO [704.5d]
    -- TODO [704.5e]
    -- TODO [704.5u]
    -- TODO [704.5v]
    -- TODO [704.5w]
  where
    checkPlayers = do
      -- [704.5a]
      -- [704.5b]
      -- TODO [704.5c]
      -- TODO [704.5t]
      ips <- IdList.toList <$> lift (gets players)
      forM_ ips $ \(i,p) -> do
        when (get life p <= 0 || get failedCardDraw p) $
          tell [Will (LoseGame i)]

    checkBattlefield = do
      ios <- IdList.toList <$> lift (gets battlefield)
      forM_ ios $ \(i,o) -> do

        -- Check creatures
        when (o `hasTypes` creatureType) $ do

          -- [704.5f]
          let hasNonPositiveToughness = maybe False (<= 0) (get toughness o)
          when hasNonPositiveToughness $ tell [willMoveToGraveyard i o]

          -- [704.5g]
          -- [704.5h]
          let hasLethalDamage =
                case (get toughness o, get damage o) of
                  (Just t, Just d) -> t > 0 && d >= t
                  _                -> False
          when (hasLethalDamage || get deathtouched o) $
            tell [Will (DestroyPermanent i True)]

        -- [704.5i]
        when (o `hasTypes` planeswalkerType && countCountersOfType Loyalty o == 0) $
          tell [willMoveToGraveyard i o]

      -- TODO [704.5j]
      -- TODO [704.5k]
      -- TODO [704.5m]
      -- TODO [704.5n]
      -- TODO [704.5p]
      -- TODO [704.5q]
      -- TODO [704.5r]
      -- TODO [704.5s]

-- | Ask players to put pending items on the stack in APNAP order. [405.3]
processPrestacks :: Engine ()
processPrestacks = do
  ips <- apnap
  forM_ ips $ \(i,p) -> do
    let pending = get prestack p
    when (not (null pending)) $ do
      pending' <- liftQuestion (AskReorder i pending)
      forM_ pending' $ \mkStackObject -> do
        stackObject <- executeMagic mkStackObject
        stack ~: IdList.cons stackObject

resolve :: Id -> Engine ()
resolve i = do
  o <- gets (stack .^ listEl i)
  let Just item = get stackItem o
  let (_, mkEffects) = evaluateTargetList item
  executeMagic mkEffects >>= mapM_ executeEffect
  -- if the object is now still on the stack, move it to the appropriate zone
  let o' = set stackItem Nothing o
  if (o `hasTypes` instantType || o `hasTypes` sorceryType)
    then moveObject (Stack, i) (Graveyard (get controller o)) o'
    else moveObject (Stack, i) Battlefield o'

object :: ObjectRef -> World :-> Object
object (zoneRef, i) = compileZoneRef zoneRef .^ listEl i

collectActions :: PlayerRef -> Engine [PriorityAction]
collectActions p = do
  objects <- executeMagic allObjects
  execWriterT $ do
    for objects $ \(r,o) -> do
      let Just playAbility = get play o
      playAbilityOk <- lift $ executeMagic (view (get available (playAbility r p)))
      when playAbilityOk (tell [PlayCard r])

      for (get activatedAbilities o) $ \ability -> do
        abilityOk <- lift $ executeMagic (view (get available (ability r p)))
        when abilityOk (tell [ActivateAbility ability])

executeAction :: Ability -> ObjectRef -> PlayerRef -> Engine ()
executeAction ability rSource activatorId = do
  let closedAbility = ability rSource activatorId
  -- TODO pay costs
  case _effect closedAbility of
    SpecialAction m -> executeMagic m >>= mapM_ executeEffect
    StackingAction _ -> return ()

-- [616] Interaction of Replacement and/or Prevention Effects
-- TODO Handle multiple effects (in a single event) at once, to be able to adhere
-- to APNAP order; see http://draw3cards.com/questions/9618
applyReplacementEffects :: OneShotEffect -> Engine [OneShotEffect]
applyReplacementEffects effect = do
    objects <- map snd <$> executeMagic allObjects
    go (concatMap (get replacementEffects) objects) effect
  where
    go :: [ReplacementEffect] -> OneShotEffect -> Engine [OneShotEffect]
    go availableEffects effectToReplace = do
      p <- affectedPlayer effectToReplace
      let (notApplicable, applicable) =
            partitionEithers $ map (\f -> maybe (Left f) (\m -> Right (f, m)) (f effectToReplace)) availableEffects
      if null applicable
        then return [effectToReplace]
        else do
          ((chosen, mReplacements), notChosen) <-
            liftQuestion (AskPickReplacementEffect p applicable)
          replacements <- executeMagic mReplacements
          -- TODO Resolve replacements in affected player APNAP order.
          fmap concat $ for replacements (go (map fst notChosen ++ notApplicable))

-- [616.1] The affected player chooses which replacement effect to apply first.
affectedPlayer :: OneShotEffect -> Engine PlayerRef
affectedPlayer e =
  case e of
    WillMoveObject o _ _          -> controllerOf o
    Will (AdjustLife p _)         -> return p
    Will (DamageObject _ o _ _ _) -> controllerOf o
    Will (DamagePlayer _ p _ _ _) -> return p
    Will (ShuffleLibrary p)       -> return p
    Will (DrawCard p)             -> return p
    Will (DestroyPermanent i _)   -> controllerOf (Battlefield, i)
    Will (TapPermanent i)         -> controllerOf (Battlefield, i)
    Will (UntapPermanent i)       -> controllerOf (Battlefield, i)
    Will (AddCounter o _)         -> controllerOf o
    Will (RemoveCounter o _)      -> controllerOf o
    Will (CreateObject o)         -> return (get controller o)
    Will (AddToManaPool p _)      -> return p
    Will (AttachPermanent o _ _)  -> controllerOf o  -- debatable
    Will (RemoveFromCombat i)     -> controllerOf (Battlefield, i)
    Will (PlayLand o)             -> controllerOf o
    Will (LoseGame p)             -> return p
  where controllerOf o = gets (object o .^ controller)

executeMagic :: Magic a -> Engine a
executeMagic m = State.get >>= lift . lift . runReaderT m

-- | Returns player IDs in APNAP order (active player, non-active player).
apnap :: Engine [(PlayerRef, Player)]
apnap = do
  activePlayerId <- gets activePlayer
  (ps, qs) <- break ((== activePlayerId) . fst) . IdList.toList <$> gets players
  return (qs ++ ps)

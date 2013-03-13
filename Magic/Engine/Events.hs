{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Magic.Engine.Events (
    -- * Executing effects
    executeMagic, executeEffects, executeEffect, raise, applyReplacementEffects,

    -- * Compiling effects
    -- | These functions all immediately execute their effect, bypassing any
    -- replacement effects that might apply to them. They will cause triggered
    -- abilities to trigger.
    compileEffect,
    untapPermanent, drawCard, moveObject, moveAllObjects, shuffleLibrary, tick
  ) where

import Magic.Core
import Magic.IdList (Id)
import qualified Magic.IdList as IdList
import Magic.Labels
import Magic.Events (willMoveToGraveyard)
import Magic.Types
import Magic.Engine.Types

import Control.Applicative ((<$>))
import Control.Monad (forM_,)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Operational (singleton, Program, ProgramT, viewT, ProgramViewT(..))
import Data.Either (partitionEithers)
import Data.Label.Pure (get, set)
import Data.Label.PureM (gets, puts, (=:), asks)
import Data.List ((\\))
import Data.Monoid ((<>))
import Data.Traversable (for)
import Prelude hiding (interact)


executeMagic :: Magic a -> Engine a
executeMagic m = do
  world <- view ask
  runExecuteEffectsProgram (runReaderT (runViewT (runMagic m)) world)

runExecuteEffectsProgram :: ProgramT ExecuteEffects (Program Interact) a -> Engine a
runExecuteEffectsProgram program = interact (viewT program) >>= eval
  where
    eval (Return x) = return x
    eval (ExecuteEffects effs :>>= k) = executeEffects effs >>= runExecuteEffectsProgram . k

-- Execute multiple effects as a single event, applying replacement effects and
-- triggering abilities.
executeEffects :: [OneShotEffect] -> Engine [Event]
executeEffects [] = return []
executeEffects effects = do
  effects' <- concat <$> for effects applyReplacementEffects

  -- If enough players lose to end the game, end the game right now
  let losingPlayers = [ p | Will (LoseGame p) <- effects' ]
  remainingPlayers <- (\\ losingPlayers) . IdList.ids <$> gets players
  case remainingPlayers of
    []  -> throwError GameDraw
    [p] -> throwError (GameWin p)
    _   -> return ()  -- continue as normal

  events <- concat <$> for effects' compileEffect

  turnHistory ~: (++ events)

  raise events
  return events

raise :: [Event] -> Engine ()
raise events = do
  world <- view ask

  interact $ singleton (LogEvents events world)

  ros <- view allObjects
  forM_ ros $ \(ro, o) -> do
    let tas = get triggeredAbilities o
    let p = get controller o
    forM_ tas $ \ta -> do
      prestackItems <- view $ do
        programs <- ta ro p events
        viewedObject <- asks (object ro)
        return (map (\program -> ((ro, viewedObject), program)) programs)
      player p .^ prestack ~: (++ prestackItems)

executeEffect :: OneShotEffect -> Engine [Event]
executeEffect = executeEffects . (: [])

-- [616] Interaction of Replacement and/or Prevention Effects
-- TODO Handle multiple effects (in a single event) at once, to be able to adhere
-- to APNAP order; see http://draw3cards.com/questions/9618
applyReplacementEffects :: OneShotEffect -> Engine [OneShotEffect]
applyReplacementEffects eff = do
    objects <- map snd <$> view allObjects
    go (concatMap (get replacementEffects) objects) eff
  where
    go :: [ReplacementEffect] -> OneShotEffect -> Engine [OneShotEffect]
    go availableEffects effectToReplace = do
      p <- affectedPlayer effectToReplace
      let (notApplicable, applicable) =
            partitionEithers $ map (\f -> maybe (Left f) (\m -> Right (f, m)) (f effectToReplace)) availableEffects
      if null applicable
        then return [effectToReplace]
        else do
          ((_, mReplacements), notChosen) <-
            askQuestion p (AskPickReplacementEffect applicable)
          replacements <- executeMagic mReplacements
          -- TODO Resolve replacements in affected player APNAP order.
          fmap concat $ for replacements (go (map fst notChosen ++ notApplicable))

-- [616.1] The affected player chooses which replacement effect to apply first.
affectedPlayer :: OneShotEffect -> Engine PlayerRef
affectedPlayer e =
  case e of
    WillMoveObject _ _ o          -> return (get controller o)
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
    Will (AddToManaPool p _)      -> return p
    Will (SpendFromManaPool p _)  -> return p
    Will (AttachPermanent o _ _)  -> controllerOf o  -- debatable
    Will (RemoveFromCombat i)     -> controllerOf (Battlefield, i)
    Will (PlayLand p _)           -> return p
    Will (LoseGame p)             -> return p
    Will (WinGame p)              -> return p
    Will (CeaseToExist o)         -> controllerOf o
  where controllerOf o = gets (object o .^ controller)



-- COMPILATION OF EFFECTS


-- | Compile and execute an effect.
compileEffect :: OneShotEffect -> Engine [Event]
compileEffect e =
  case e of
    WillMoveObject mrObj rToZone obj -> moveObject mrObj rToZone obj
    Will (TapPermanent i)           -> tapPermanent i
    Will (UntapPermanent i)         -> untapPermanent i
    Will (DrawCard rp)              -> drawCard rp
    Will (DestroyPermanent i reg)   -> destroyPermanent i reg
    Will (ShuffleLibrary rPlayer)   -> shuffleLibrary rPlayer
    Will (PlayLand p ro)            -> playLand p ro
    Will (AddToManaPool p pool)     -> addToManaPool p pool
    Will (SpendFromManaPool p pool) -> spendFromManaPool p pool
    Will (DamageObject source r amount isCombatDamage isPreventable) -> damageObject source r amount isCombatDamage isPreventable
    Will (DamagePlayer source p amount isCombatDamage isPreventable) -> damagePlayer source p amount isCombatDamage isPreventable
    Will (LoseGame p)               -> loseGame p
    Will (WinGame p)                -> winGame p
    Will (CeaseToExist r)           -> ceaseToExist r
    _ -> error "compileEffect: effect not implemented"

tapPermanent :: Id -> Engine [Event]
tapPermanent i = do
  Just ts <- gets (object (Battlefield, i) .^ tapStatus)
  case ts of
    Untapped -> do
      object (Battlefield, i) .^ tapStatus =: Just Tapped
      return [Did (TapPermanent i)]
    Tapped   -> return []

-- | Cause a permanent on the battlefield to untap. If it was previously tapped, a 'Did' 'UntapPermanent' event is raised.
untapPermanent :: Id -> Engine [Event]
untapPermanent i = do
  Just ts <- gets (object (Battlefield, i) .^ tapStatus)
  case ts of
    Untapped -> return []
    Tapped -> do
      object (Battlefield, i) .^ tapStatus =: Just Untapped
      return [Did (UntapPermanent i)]

-- | Cause the given player to draw a card. If a card was actually drawn, a 'Did' 'DrawCard' event is raised. If not, the player loses the game the next time state-based actions are checked.
drawCard :: PlayerRef -> Engine [Event]
drawCard rp = do
  lib <- gets (players .^ listEl rp .^ library)
  case IdList.toList lib of
    []          -> do
      players .^ listEl rp .^ failedCardDraw =: True
      return []
    (ro, o) : _ -> do
      events <- compileEffect (WillMoveObject (Just (Library rp, ro)) (Hand rp) o)
      return (events ++ [Did (DrawCard rp)])

destroyPermanent :: Id -> Bool -> Engine [Event]
destroyPermanent i reg = do
  let r = (Battlefield, i)
  o <- gets (object r)
  events <- compileEffect (willMoveToGraveyard i o)
  return (events ++ [Did (DestroyPermanent i reg)])


-- | Cause an object to move from one zone to another in the specified form. If the object was actually moved, a 'DidMoveObject' event is raised.
moveObject :: Maybe ObjectRef -> ZoneRef -> Object -> Engine [Event]
moveObject mOldRef rToZone obj =
    case mOldRef of
      Nothing -> createObject
      Just (rFromZone, i) -> do
        mObj <- IdList.removeM (compileZoneRef rFromZone) i
        case mObj of
          Nothing -> return []
          Just _  -> createObject
  where
    createObject = do
      t <- tick
      let insertOp
            | rToZone == Stack  = IdList.consM
            | otherwise         = IdList.snocM
      newId <- insertOp (compileZoneRef rToZone) (set timestamp t obj)
      return [DidMoveObject mOldRef (rToZone, newId)]

-- | @moveAllObjects z1 z2@ moves all objects from zone @z1@ to zone @z2@, raising a 'DidMoveObject' event for every object that was moved this way.
moveAllObjects :: ZoneRef -> ZoneRef -> Engine [Event]
moveAllObjects rFromZone rToZone = do
  ois <- IdList.toList <$> gets (compileZoneRef rFromZone)
  concat <$> (for ois $ \(i, o) -> moveObject (Just (rFromZone, i)) rToZone o)

-- | Shuffle a player's library. A 'ShuffleLibrary' event is raised.
shuffleLibrary :: PlayerRef -> Engine [Event]
shuffleLibrary rPlayer = do
  let libraryLabel = players .^ listEl rPlayer .^ library
  lib <- gets libraryLabel
  lib' <- IdList.shuffle lib
  puts libraryLabel lib'
  return [Did (ShuffleLibrary rPlayer)]

playLand :: PlayerRef -> ObjectRef -> Engine [Event]
playLand p ro = do
  o <- gets (object ro)
  -- TODO apply replacement effects on the move effect
  -- TODO store more sensible data in the PlayLand event
  events <- moveObject (Just ro) Battlefield o { _tapStatus = Just Untapped, _controller = p }
  return (events ++ [Did (PlayLand p ro)])

addToManaPool :: PlayerRef -> ManaPool -> Engine [Event]
addToManaPool p pool = do
  player p .^ manaPool ~: (pool <>)
  return [Did (AddToManaPool p pool)]

spendFromManaPool :: PlayerRef -> ManaPool -> Engine [Event]
spendFromManaPool p pool = do
  player p .^ manaPool ~: (\\ pool)
  return [Did (SpendFromManaPool p pool)]

damageObject :: Object -> ObjectRef -> Int -> Bool -> Bool -> Engine [Event]
-- 119.8. If a source would deal 0 damage, it does not deal damage at all.
damageObject _ _ 0 _ _ = return []
damageObject source r amount isCombatDamage isPreventable = do
  -- TODO check for protection, infect, wither, lifelink
  object r .^ damage ~: (+ amount)
  return [Did (DamageObject source r amount isCombatDamage isPreventable)]

damagePlayer :: Object -> PlayerRef -> Int -> Bool -> Bool -> Engine [Event]
-- 119.8. If a source would deal 0 damage, it does not deal damage at all.
damagePlayer _ _ 0 _ _ = return []
damagePlayer source p amount isCombatDamage isPreventable = do
  -- TODO check for protection, infect, wither, lifelink
  player p .^ life ~: subtract amount
  return [Did (DamagePlayer source p amount isCombatDamage isPreventable)]

loseGame :: PlayerRef -> Engine [Event]
loseGame p = do
  -- TODO Remove all objects that belong to the player
  ps <- gets players
  case IdList.remove p ps of
    Nothing        -> return []
    Just (_, ps')  -> do
      players =: ps'
      return [Did (LoseGame p)]

winGame :: PlayerRef -> Engine a
winGame p = throwError (GameWin p)

ceaseToExist :: ObjectRef -> Engine [Event]
ceaseToExist r@(z, i) = do
  m <- IdList.removeM (compileZoneRef z) i
  case m of
    Nothing -> return []
    Just _ -> return [Did (CeaseToExist r)]

tick :: Engine Timestamp
tick = do
  t <- gets time
  time ~: succ
  return t

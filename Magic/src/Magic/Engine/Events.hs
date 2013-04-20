{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Magic.Engine.Events (
    -- * Executing effects
    executeMagic, executeEffects, executeEffect, raise, applyReplacementEffects,
    compileEffect, moveAllObjects, tick
  ) where

import Magic.Core
import qualified Magic.IdList as IdList
import Magic.Labels
import Magic.Events (willMoveToGraveyard)
import Magic.Types
import Magic.Engine.Types

import Control.Applicative ((<$>), (<$))
import Control.Monad (forM_,)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Operational (singleton, Program, ProgramT, viewT, ProgramViewT(..))
import Data.Label.Pure (get, set)
import Data.Label.PureM (gets, puts, (=:), asks)
import Data.List ((\\))
import Data.Monoid ((<>))
import Data.Traversable (for)
import Prelude hiding (interact)


executeMagic :: EventSource -> Magic a -> Engine a
executeMagic source m = do
  world <- view ask
  runExecuteEffectsProgram source (runReaderT (runViewT (runMagic m)) world)

runExecuteEffectsProgram :: EventSource -> ProgramT ExecuteEffects (Program Interact) a -> Engine a
runExecuteEffectsProgram source program = interact (viewT program) >>= eval
  where
    eval (Return x) = return x
    eval (ExecuteEffects effs :>>= k) = executeEffects source effs >>= runExecuteEffectsProgram source . k

-- Execute multiple effects as a single event, applying replacement effects and
-- triggering abilities.
executeEffects :: EventSource -> [OneShotEffect] -> Engine [Event]
executeEffects _ [] = return []
executeEffects source effects = do
  effects' <- concat <$> for effects (applyReplacementEffects source)

  -- If enough players lose to end the game, end the game right now
  let losingPlayers = [ p | Will (LoseGame p) <- effects' ]
  remainingPlayers <- (\\ losingPlayers) . IdList.ids <$> gets players
  case remainingPlayers of
    []  -> throwError GameDraw
    [p] -> throwError (GameWin p)
    _   -> return ()  -- continue as normal

  events <- concat <$> for effects' compileEffect

  turnHistory ~: (++ events)

  raise source events
  return events

raise :: EventSource -> [Event] -> Engine ()
raise source events = do
  world <- view ask

  interact $ singleton (LogEvents source events world)

  ros <- view allObjects
  forM_ ros $ \(ro, o) -> do
    let tas = get triggeredAbilities o
    let p = get controller o
    prestackItems <- view $ do
      programs <- tas events ro p
      viewedObject <- asks (object ro)
      return (map (\program -> ((ro, viewedObject), program)) programs)
    player p .^ prestack ~: (++ prestackItems)

executeEffect :: EventSource -> OneShotEffect -> Engine [Event]
executeEffect source = executeEffects source . (: [])

-- [616] Interaction of Replacement and/or Prevention Effects
-- TODO Handle multiple effects (in a single event) at once, to be able to adhere
-- to APNAP order; see http://draw3cards.com/questions/9618
applyReplacementEffects :: EventSource -> OneShotEffect -> Engine [OneShotEffect]
applyReplacementEffects _ eff = return [eff]
  --applyReplacementEffects source eff = do
  --  objects <- map snd <$> view allObjects
  --  go (concatMap (get replacementEffects) objects) eff
  --where
  --  go :: [ReplacementEffect] -> OneShotEffect -> Engine [OneShotEffect]
  --  go availableEffects effectToReplace = do
  --    p <- affectedPlayer effectToReplace
  --    let (notApplicable, applicable) =
  --          partitionEithers $ map (\f -> maybe (Left f) (\m -> Right (f, m)) (f effectToReplace)) availableEffects
  --    if null applicable
  --      then return [effectToReplace]
  --      else do
  --        ((_, mReplacements), notChosen) <-
  --          askQuestion p (AskPickReplacementEffect applicable)
  --        replacements <- executeMagic undefined mReplacements
  --        -- TODO Resolve replacements in affected player APNAP order.
  --        fmap concat $ for replacements (go (map fst notChosen ++ notApplicable))

-- [616.1] The affected player chooses which replacement effect to apply first.
affectedPlayer :: OneShotEffect -> Engine PlayerRef
affectedPlayer e =
  case e of
    WillMoveObject _ _ o          -> return (get controller o)
    Will (GainLife p _)           -> return p
    Will (LoseLife p _)           -> return p
    Will (DamageObject _ i _ _ _) -> controllerOf (Battlefield, i)
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
    Will (InstallLayeredEffect r _) -> controllerOf r
    Will (CeaseToExist o)         -> controllerOf o
  where controllerOf o = gets (object o .^ controller)



-- COMPILATION OF EFFECTS


-- | Compile and execute an effect.
compileEffect :: OneShotEffect -> Engine [Event]
compileEffect e =
  case e of
    WillMoveObject mOldRef rToZone obj ->
      let createObject = do
            t <- tick
            let insertOp
                  | rToZone == Stack  = IdList.consM
                  | otherwise         = IdList.snocM
            newId <- insertOp (compileZoneRef rToZone) (set timestamp t obj)
            return [DidMoveObject mOldRef (rToZone, newId)]
      in case mOldRef of
        -- TODO 303.4f-g Auras entering the battlefield without being cast
        Nothing -> createObject
        Just (rFromZone, i) -> do
          mObj <- IdList.removeM (compileZoneRef rFromZone) i
          case mObj of
            Nothing -> return []
            Just _  -> createObject

    Will simpleEffect ->
      let simply      = ([Did simpleEffect] <$)
          combine eff = (++ [Did simpleEffect]) <$> compileEffect eff
      in case simpleEffect of

        GainLife p n
          | n <= 0    -> return []
          | otherwise -> simply $ player p .^ life ~: (+ n)

        LoseLife p n
          | n <= 0    -> return []
          | otherwise -> simply $ player p .^ life ~: (subtract n)

        TapPermanent i -> do
          Just ts <- gets (object (Battlefield, i) .^ tapStatus)
          case ts of
            Untapped -> simply $ object (Battlefield, i) .^ tapStatus =: Just Tapped
            Tapped   -> return []

        UntapPermanent i -> do
          Just ts <- gets (object (Battlefield, i) .^ tapStatus)
          case ts of
            Untapped -> return []
            Tapped   -> simply $ object (Battlefield, i) .^ tapStatus =: Just Untapped

        DrawCard rp -> do
          lib <- gets (players .^ listEl rp .^ library)
          case IdList.toList lib of
            [] -> do
              players .^ listEl rp .^ failedCardDraw =: True
              return []
            (ro, o) : _ ->
              combine $ WillMoveObject (Just (Library rp, ro)) (Hand rp) o

        DestroyPermanent i _ -> do
          o <- gets (object (Battlefield, i))
          combine $ willMoveToGraveyard i o

        ShuffleLibrary rPlayer -> simply $ do
          let libraryLabel = players .^ listEl rPlayer .^ library
          lib <- gets libraryLabel
          lib' <- IdList.shuffle lib
          puts libraryLabel lib'

        PlayLand p ro -> do
          o <- gets (object ro)
          -- TODO apply replacement effects on the move effect
          -- TODO store more sensible data in the PlayLand event
          combine $ WillMoveObject (Just ro) Battlefield o { _tapStatus = Just Untapped, _controller = p }

        AddToManaPool p pool ->
          simply $ player p .^ manaPool ~: (pool <>)

        SpendFromManaPool p pool ->
          simply $ player p .^ manaPool ~: (\\ pool)

        DamageObject _source i amount _isCombatDamage _isPreventable ->
          -- TODO check for protection, infect, wither, lifelink
          simply $ object (Battlefield, i) .^ damage ~: (+ amount)

        DamagePlayer _source p amount _isCombatDamage _isPreventable ->
          -- TODO check for protection, infect, wither, lifelink
          simply $ player p .^ life ~: subtract amount

        LoseGame p -> do
          -- TODO Remove all objects that belong to the player
          ps <- gets players
          case IdList.remove p ps of
            Nothing        -> return []
            Just (_, ps')  -> simply $ players =: ps'

        WinGame p ->
          throwError (GameWin p)

        CeaseToExist (z, i) -> do
          m <- IdList.removeM (compileZoneRef z) i
          case m of
            Nothing -> return []
            Just _  -> simply $ return ()

        _ -> error "compileEffect: effect not implemented"

moveAllObjects :: EventSource -> ZoneRef -> ZoneRef -> Engine [Event]
moveAllObjects source rFromZone rToZone = do
  idxs <- IdList.toList <$> gets (compileZoneRef rFromZone)
  executeEffects source (map (\(i, x) -> WillMoveObject (Just (rFromZone, i)) rToZone x) idxs)

tick :: Engine Timestamp
tick = do
  t <- gets time
  time ~: succ
  return t

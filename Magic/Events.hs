module Magic.Events (
    -- * Types
    Event(..), OneShotEffect(..), SimpleOneShotEffect(..),

    -- * Executing effects
    executeEffect, raise, applyReplacementEffects, compileEffect,

    -- * Specific effects
    untapPermanent, drawCard, moveObject, moveAllObjects, shuffleLibrary, tick
  ) where

import Magic.Core
import Magic.IdList (Id)
import qualified Magic.IdList as IdList
import Magic.Labels
import Magic.Types

import Control.Applicative ((<$>))
import Control.Monad (forM_,)
import Control.Monad.Trans (lift)
import Data.Either (partitionEithers)
import Data.Label.Pure (get, set)
import Data.Label.PureM (gets, puts, (=:))
import Data.Traversable (for)



-- | Execute a one-shot effect, applying replacement effects and triggering abilities.
-- TODO Return [Event] what actually happened
executeEffect :: OneShotEffect -> Engine ()
executeEffect e = applyReplacementEffects e >>= mapM_ compileEffect

raise :: Event -> Engine ()
raise _ = do
  -- TODO handle triggered abilities
  return ()


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


-- Compilation of effects

-- TODO Return [Event] what actually happened
compileEffect :: OneShotEffect -> Engine ()
compileEffect e =
  case e of
    WillMoveObject rObj rToZone obj -> moveObject rObj rToZone obj
    Will (UntapPermanent i)         -> untapPermanent i
    Will (DrawCard rp)              -> drawCard rp
    Will (ShuffleLibrary rPlayer)   -> shuffleLibrary rPlayer
    _ -> undefined

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

tick :: Engine Timestamp
tick = do
  t <- gets time
  time ~: succ
  return t

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Magic.Engine.Types (Engine(..), GameOver(..)) where

import Magic.Layers
import Magic.Types
import Magic.Core (allObjects, objectBase)
import Magic.Utils

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad.Error (MonadError(..), Error(..))
import Control.Monad.Identity
import Control.Monad.Random (MonadRandom, RandT, StdGen)
import Control.Monad.Reader
import Control.Monad.State (StateT, MonadState(..))
import Control.Monad.Operational (ProgramT, liftProgram)
import Data.Label.Pure (set, modify)
import Data.List (delete)
import Data.Monoid ((<>), mempty)
import Data.Text (Text, pack)
import Prelude hiding (interact)


newtype Engine a = Engine { runEngine :: StateT World (RandT StdGen (ProgramT Interact (Either GameOver))) a }
  deriving (Functor, Applicative, MonadState World, MonadRandom)

instance Monad Engine where
  return         = Engine . return
  Engine x >>= f = Engine (x >>= (runEngine . f))
  fail           = throwError . strMsg

instance MonadView Engine where
  view (ViewT f) = runReader f <$> applyLayeredEffects

instance MonadInteract Engine where
  interact = Engine . lift . lift . liftProgram

instance MonadError GameOver Engine where
  throwError = Engine . lift . lift . lift . throwError
  catchError = error "not yet implemented: Engine catchError"

data GameOver
  = GameWin PlayerRef
  | GameDraw
  | ErrorWithMessage Text
  | UnknownError

instance Error GameOver where
  noMsg  = UnknownError
  strMsg = ErrorWithMessage . pack

applyLayeredEffects :: Engine World
applyLayeredEffects = do
    -- TODO Losing all abilities might cause layered effects to disappear, so don't collect them all beforehand.
    -- TODO Detect and handle dependencies.
    world <- get
    let ros = runView allObjects world
    return (applyAll (sortedEffects ros) world)
  where
    allEffects os =
      [ (t, vas, m)
      | (r, o) <- os
      , let p = _controller o
      , let inherentTuples = [ (_timestamp o, vas, ms)
              | LayeredEffect as ms <- _layeredEffects o, let vas = as r p ]
      , let temporaryTuples = [ (t, vas, ms)
              | TemporaryLayeredEffect t _ (LayeredEffect as ms)
                  <- _temporaryEffects o, let vas = as r p ]
      , (t, vas, ms) <- inherentTuples ++ temporaryTuples
      , m <- ms ]

    sortedEffects os = sortOn (\(t, _, m) -> (layer m, t)) (allEffects os)

    applyAll :: [(Timestamp, View [SomeObjectRef], ModifyObject)] -> World -> World
    applyAll [] world = world
    applyAll ((_, vas, m) : ts) world =
        applyAll ts (applyOne affected m world)
      where
        affected = runReader (runViewT vas) world

    applyOne :: [SomeObjectRef] -> ModifyObject -> World -> World
    applyOne rs m world = foldr (.) id (map (\r -> modify (objectBase r) (compileModifyObject world m)) rs) world

compileModifyObject :: World -> ModifyObject -> Object -> Object
compileModifyObject world m =
  case m of
    ChangeController p -> set controller p
    ChangeTypes f -> modify types f
    ChangeColors f -> modify colors f
    AddStaticKeywordAbility ab -> modify staticKeywordAbilities (++ [ab])
    RemoveStaticKeywordAbility ab -> modify staticKeywordAbilities (delete ab)
    AddActivatedAbility ab -> modify activatedAbilities (++ [ab])
    AddTriggeredAbilities as -> modify triggeredAbilities (<> as)
    RemoveAllAbilities -> set activatedAbilities []
                        . set triggeredAbilities mempty
                        . set staticKeywordAbilities []
                        . set layeredEffects []
    DefinePT vpt -> set pt (Just (runReader (runViewT vpt) world))
    SetPT newPT -> set pt (Just newPT)
    ModifyPT vpt -> let (p, t) = runReader (runViewT vpt) world
                    in modify pt (fmap ((+ p) *** (+ t)))
    SwitchPT -> modify pt (fmap (\(p,t) -> (t,p)))

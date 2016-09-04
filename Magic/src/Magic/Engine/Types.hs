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
import Control.Monad.Except (MonadError(..))
import Control.Monad.Identity
import Control.Monad.Random (MonadRandom, RandT, StdGen)
import Control.Monad.Reader
import Control.Monad.State (StateT, MonadState(..))
import Control.Monad.Operational (ProgramT, liftProgram)
import Data.Boolean ((&&*))
import Data.Label (set, modify)
import Data.List (delete)
import Data.Monoid ((<>), mempty)
import Data.Text (Text, pack)
import Prelude hiding (interact)


newtype Engine a = Engine { runEngine :: StateT World (RandT StdGen (ProgramT Interact (Either GameOver))) a }
  deriving (Functor, Applicative, MonadState World, MonadRandom)

instance Monad Engine where
  return         = Engine . return
  Engine x >>= f = Engine (x >>= (runEngine . f))

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

applyLayeredEffects :: Engine World
applyLayeredEffects = do
    -- TODO Losing all abilities might cause layered effects to disappear, so don't collect them all beforehand.
    -- TODO Detect and handle dependencies.
    world <- get
    let ros = runView allObjects world
    return (applyAll (sortedEffects ros) world)
  where
    allEffects :: [(SomeObjectRef, Object)] ->
      [(Timestamp, View [SomeObjectRef], ModifyObject)]
    allEffects os =
      [ (t, vas, m)
      | (r, o) <- os
      , let p = _controller o
      , let inherentTuples :: [(Timestamp, View [SomeObjectRef],
                                [ModifyObject])]
            inherentTuples = [ (_timestamp o, vas, ms)
              | LayeredObjectEffect as ms <- _layeredEffects o
              , let vas = as r p ]
      , let temporaryTuples :: [(Timestamp, View [SomeObjectRef],
                                [ModifyObject])]
            temporaryTuples = [ (t, vas, ms)
              | TemporaryLayeredEffect t _ (LayeredObjectEffect as ms)
                  <- _temporaryEffects o, let vas = as r p ]
      , (t, vas, ms) <- inherentTuples ++ temporaryTuples ++
                          [counterEffect (r, o)]
      , m <- ms ]

    sortedEffects :: [(SomeObjectRef, Object)] ->
      [(Timestamp, View [SomeObjectRef], ModifyObject)]
    sortedEffects os = sortOn (\(t, _, m) -> (layer m, t)) (allEffects os)

    applyAll :: [(Timestamp, View [SomeObjectRef], ModifyObject)] -> World -> World
    applyAll [] world = world
    applyAll ((_, vas, m) : ts) world =
        applyAll ts (applyOne affected m world)
      where
        affected = runReader (runViewT vas) world

    applyOne :: [SomeObjectRef] -> ModifyObject -> World -> World
    applyOne rs m world = foldr (.) id (map (\r -> modify (objectBase r) (compileModifyObject world m r)) rs) world

compileModifyObject :: World -> ModifyObject -> SomeObjectRef -> Object -> Object
compileModifyObject world m rSelf =
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
    DefinePT vpt -> set pt (Just (runReader (runViewT (vpt rSelf)) world))
    SetPT newPT -> set pt (Just newPT)
    ModifyPT vpt -> let (p, t) = runReader (runViewT (vpt rSelf)) world
                    in modify pt (fmap ((+ p) *** (+ t)))
    SwitchPT -> modify pt (fmap (\(p,t) -> (t,p)))
    RestrictAllowAttacks ok -> modify allowAttacks (&&* ok)
    RestrictAllowBlocks ok -> modify allowBlocks (&&* ok)

counterEffect :: (SomeObjectRef, Object) ->
  (Timestamp, View [SomeObjectRef], [ModifyObject])
counterEffect (r, o) = (0, return [r], [ModifyPT (\_ -> return (n, n))])
  where
    nplus  = length [ () | Plus1Plus1   <- _counters o ]
    nminus = length [ () | Minus1Minus1 <- _counters o ]
    n      = nplus - nminus

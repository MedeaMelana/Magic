{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Magic.Events (
    -- * Types
    OneShotEffect(..), SimpleOneShotEffect(..), Event(..), Attack(..),

    -- * Constructing specific one-shot effects
    willMoveToGraveyard, willMoveToStack, -- willMoveToBattlefield,

    executeEffects, executeEffect, will,
    tick
  ) where

import Magic.Some (Some(..))
import Magic.Core
import Magic.Types

import Data.Functor (void)
import Control.Monad.Operational (singleton)
import Control.Monad.Trans (lift)
import Data.Label (get)
import Data.Label.Monadic (asks)
import Prelude hiding (interact)



-- CONSTRUCTING SPECIFIC ONE-SHOT EFFECTS


-- | Effect that moves the specified object on the battlefield to its owner's graveyard.
willMoveToGraveyard :: ObjectRef TyPermanent -> Object -> OneShotEffect
willMoveToGraveyard (Battlefield, i) o = WillMoveObject (Just (Some Battlefield, i)) (Graveyard (get owner o)) (CardObject o)

--willMoveToBattlefield :: SomeObjectRef -> View OneShotEffect
--willMoveToBattlefield r = do
--  o <- asks (object r)
--  let o' = o { _tapStatus = Just Untapped }
--  return (WillMoveObject (Just r) Battlefield o')

willMoveToStack :: SomeObjectRef -> StackItem -> View OneShotEffect
willMoveToStack r si = do
  o <- asks (objectBase r)
  return (WillMoveObject (Just r) Stack (StackItem o si))



-- EXECUTING EFFECTS


executeEffects :: [OneShotEffect] -> Magic [Event]
executeEffects = Magic . lift . singleton . ExecuteEffects

executeEffect :: OneShotEffect -> Magic [Event]
executeEffect = executeEffects . (: [])

-- Execute a single simple effect in a single event, discarding the result.
will :: SimpleOneShotEffect -> Magic ()
will eff = void (executeEffect (Will eff))

tick :: Magic Timestamp
tick = Magic $ lift $ singleton $ Tick

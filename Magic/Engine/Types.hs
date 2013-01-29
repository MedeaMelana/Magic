{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Magic.Engine.Types where

import Magic.Types

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Random (MonadRandom, RandT, StdGen)
import Control.Monad.Reader
import Control.Monad.State (MonadState, StateT, get)
import qualified Control.Monad.State as State
import qualified Control.Monad.Operational as Operational
import Prelude hiding (interact)


newtype Engine a = Engine { runEngine :: StateT World (RandT StdGen (Operational.Program Interact)) a }
  deriving (Functor, Applicative, Monad, MonadState World, MonadRandom)

instance MonadView Engine where
  -- TODO Apply continuous effects
  view (ViewT (ReaderT f)) = liftM (runIdentity . f) get

instance MonadInteract Engine where
  interact = executeMagic . interact

executeMagic :: Magic a -> Engine a
executeMagic (Magic m) = Engine (State.get >>= lift . lift . runReaderT (runViewT m))

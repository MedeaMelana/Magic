{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module Magic.Engine.Types where

import Magic.Types

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Random (MonadRandom, RandT, StdGen)
import Control.Monad.Reader
import Control.Monad.State (MonadState, StateT, get)
import qualified Control.Monad.State as State
import Control.Monad.Operational (ProgramT, ProgramViewT(..), Program)
import qualified Control.Monad.Operational as Operational
import Prelude hiding (interact)


newtype Engine a = Engine { runEngine :: StateT World (RandT StdGen (Program Interact)) a }
  deriving (Functor, Applicative, Monad, MonadState World, MonadRandom)

instance MonadView Engine where
  -- TODO Apply continuous effects
  view (ViewT (ReaderT f)) = liftM (runIdentity . f) get

instance MonadInteract Engine where
  interact = Engine . lift . lift

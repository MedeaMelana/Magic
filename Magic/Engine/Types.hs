{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module Magic.Engine.Types where

import Magic.Types

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Random (MonadRandom, RandT, StdGen)
import Control.Monad.Reader
import Control.Monad.State (StateT, MonadState(..))
import Control.Monad.Operational (Program)
import Prelude hiding (interact)


newtype Engine a = Engine { runEngine :: StateT World (RandT StdGen (Program Interact)) a }
  deriving (Functor, Applicative, Monad, MonadState World, MonadRandom)

instance MonadView Engine where
  -- TODO Apply continuous effects
  view (ViewT (ReaderT f)) = liftM (runIdentity . f) get

instance MonadInteract Engine where
  interact = Engine . lift . lift

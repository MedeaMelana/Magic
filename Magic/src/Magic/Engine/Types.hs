{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Magic.Engine.Types where

import Magic.Types

import Control.Applicative
import Control.Monad.Error (MonadError(..), Error(..))
import Control.Monad.Identity
import Control.Monad.Random (MonadRandom, RandT, StdGen)
import Control.Monad.Reader
import Control.Monad.State (StateT, MonadState(..))
import Control.Monad.Operational (ProgramT, liftProgram)
import Data.Text (Text, pack)
import Prelude hiding (interact)


newtype Engine a = Engine { runEngine :: StateT World (RandT StdGen (ProgramT Interact (Either GameOver))) a }
  deriving (Functor, Applicative, MonadState World, MonadRandom)

instance Monad Engine where
  return         = Engine . return
  Engine x >>= f = Engine (x >>= (runEngine . f))
  fail           = throwError . strMsg

instance MonadView Engine where
  -- TODO Apply continuous effects
  view (ViewT (ReaderT f)) = liftM (runIdentity . f) get

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

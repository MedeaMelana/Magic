{-# LANGUAGE TypeOperators #-}

module Labels where

import Types

import Prelude hiding ((.), id)
import Control.Category (Category(..), (>>>))
import Control.Monad.State (MonadState)
import Data.Label.Pure ((:->), lens)
import Data.Label.PureM
import qualified Data.IntMap as IntMap


ref :: Ref a -> RefMap a :-> a
ref key = lens (IntMap.! key) (IntMap.insert key)

(.^) :: Category (~>) => a ~> b -> b ~> c -> a ~> c
(.^) = (>>>)

(~:) :: MonadState s m => (s :-> a) -> (a -> a) -> m ()
(~:) = modify

(~:*) :: (Functor f, MonadState s m) => (s :-> f a) -> (a -> a) -> m ()
l ~:* f = l ~: fmap f

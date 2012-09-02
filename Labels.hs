{-# LANGUAGE TypeOperators #-}

module Labels where

import IdList (Id, IdList)
import qualified IdList
import Types

import Prelude hiding ((.), id)
import Control.Category (Category(..), (>>>))
import Control.Monad.State (MonadState)
import Data.Label.Pure ((:->), lens)
import Data.Label.PureM
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust)


listEl :: Id -> IdList a :-> a
listEl i = lens (fromJust . IdList.get i) (IdList.set i)

mapEl :: Id -> IdMap a :-> a
mapEl i = lens (IntMap.! i) (IntMap.insert i)

(.^) :: Category (~>) => a ~> b -> b ~> c -> a ~> c
(.^) = (>>>)

(~:) :: MonadState s m => (s :-> a) -> (a -> a) -> m ()
(~:) = modify

(~:*) :: (Functor f, MonadState s m) => (s :-> f a) -> (a -> a) -> m ()
l ~:* f = l ~: fmap f

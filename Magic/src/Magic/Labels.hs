{-# LANGUAGE TypeOperators #-}

module Magic.Labels (listEl, (.^), (~:), (~:*)) where

import Magic.IdList (Id, IdList)
import qualified Magic.IdList as IdList

import Prelude hiding ((.), id)
import Control.Category (Category(..), (>>>))
import Control.Monad.State (MonadState)
import Data.Label ((:->), lens)
import Data.Label.Monadic
import Data.Maybe (fromJust)


listEl :: Id -> IdList a :-> a
listEl i = lens (fromJust . IdList.get i) (IdList.modify i)

(.^) :: Category cat => cat a b -> cat b c -> cat a c
(.^) = (>>>)

(~:) :: MonadState s m => (s :-> a) -> (a -> a) -> m ()
(~:) = modify

(~:*) :: (Functor f, MonadState s m) => (s :-> f a) -> (a -> a) -> m ()
l ~:* f = l ~: fmap f

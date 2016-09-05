{-# LANGUAGE TypeOperators #-}

module Magic.Labels (listEl, (=.*), (=+)) where

import Magic.IdList (Id, IdList)
import qualified Magic.IdList as IdList

import Prelude hiding ((.), id)
import Control.Category (Category(..))
import Control.Monad.State (MonadState)
import Data.Label ((:->), lens)
import Data.Label.Monadic
import Data.Maybe (fromJust)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet


listEl :: Id -> IdList a :-> a
listEl i = lens (fromJust . IdList.get i) (IdList.modify i)

(=.*) :: (Functor f, MonadState s m) => (s :-> f a) -> (a -> a) -> m ()
l =.* f = l =. fmap f

(=+) :: (MonadState s m, Ord a) => (s :-> MultiSet a) -> [a] -> m ()
l =+ xs = l =. foldr (\x f -> f . MultiSet.insert x) id xs

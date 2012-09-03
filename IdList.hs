{-# LANGUAGE TypeOperators #-}

module IdList
  ( Id, IdList
  , empty, get, set, remove, cons, toList, filter, shuffle
  , consM, removeM, shuffleM
  ) where

import Prelude hiding (filter)
import qualified Prelude

import Control.Arrow (second)
import Control.Monad.Random (MonadRandom)
import Control.Monad.State (MonadState)
import Data.Label.Pure ((:->))
import Data.Label.PureM (gets, puts)
import System.Random.Shuffle (shuffleM)

type Id = Int

data IdList a = IdList [(Id, a)] Id

instance Functor IdList where
  fmap = contents . fmap . second

empty :: IdList a
empty = IdList [] 0

get :: Id -> IdList a -> Maybe a
get i (IdList ixs _) = lookup i ixs

set :: Id -> a -> IdList a -> IdList a
set i x (IdList ixs ni) = IdList (map f ixs) ni
  where
    f ix'@(i', _)
      | i == i'    = (i, x)
      | otherwise  = ix'

remove :: Id -> IdList a -> Maybe (a, IdList a)
remove i l =
  case get i l of
    Just x  -> Just (x, contents (Prelude.filter (\(i', _) -> i /= i')) l)
    Nothing -> Nothing

cons :: a -> IdList a -> (Id, IdList a)
cons x (IdList ixs i) = (i, IdList ((i, x) : ixs) (succ i))

contents :: ([(Id, a)] -> [(Id, b)]) -> IdList a -> IdList b
contents f (IdList ixs i) = IdList (f ixs) i

toList :: IdList a -> [(Id, a)]
toList (IdList ixs _) = ixs

filter :: (a -> Bool) -> IdList a -> [(Id, a)]
filter f = Prelude.filter (f . snd) . toList

shuffle :: MonadRandom m => IdList a -> m (IdList a)
shuffle (IdList ixs ni) = do
  ixs' <- shuffleM ixs
  return (IdList ixs' ni)

removeM :: MonadState s m => (s :-> IdList a) -> Id -> m (Maybe a)
removeM label i = do
  list <- gets label
  case remove i list of
    Just (x, list') -> do puts label list'; return (Just x)
    Nothing         -> return Nothing

consM :: MonadState s m => (s :-> IdList a) -> a -> m Id
consM label x = do
  list <- gets label
  let (i, list') = cons x list
  puts label list'
  return i

{-# LANGUAGE TypeOperators #-}

module Magic.IdList (

    -- * Types
    Id, idToInt, IdList,

    -- * Construction
    empty, fromList, fromListWithId,

    -- * Querying
    length, null, head, get, toList, ids, elems,

    -- * Modifying
    set, remove, cons, cons', snoc, snoc', filter, shuffle,
    consM, snocM, removeM, shuffleM

  ) where

import Prelude hiding (length, null, filter, head)
import qualified Prelude

import Control.Arrow (second)
import Control.Monad.Random (MonadRandom)
import Control.Monad.State (MonadState)
import Data.Label.Pure ((:->))
import Data.Label.PureM (gets, puts)
import System.Random.Shuffle (shuffleM)



-- TYPES


newtype Id = Id Int
  deriving (Eq, Ord)

idToInt :: Id -> Int
idToInt (Id i) = i

data IdList a = IdList [(Id, a)] Id

instance Functor IdList where
  fmap = contents . fmap . second

instance Show Id where
  show (Id i) = show i



-- CONSTRUCTION


empty :: IdList a
empty = IdList [] (Id 0)

fromList :: [a] -> IdList a
fromList = foldr (\x xs -> snd (snoc' (const x) xs)) empty

fromListWithId :: (Id -> a -> b) -> [a] -> IdList b
fromListWithId f = foldr (\x xs -> snd (snoc' (\i -> f i x) xs)) empty



-- QUERYING


length :: IdList a -> Int
length (IdList ixs _) = Prelude.length ixs

null :: IdList a -> Bool
null (IdList ixs _) = Prelude.null ixs

head :: IdList a -> Maybe (Id, a)
head (IdList (ix : _) _) = Just ix
head _ = Nothing

get :: Id -> IdList a -> Maybe a
get i (IdList ixs _) = lookup i ixs

toList :: IdList a -> [(Id, a)]
toList (IdList ixs _) = ixs

ids :: IdList a -> [Id]
ids = map fst . toList

elems :: IdList a -> [a]
elems = map snd . toList



-- MODIFYING


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

--pop :: IdList a -> Maybe (a, IdList a)
--pop (IdList ((_, x) : ixs) i) = Just (x, IdList ixs i)
--pop _ = Nothing

cons :: a -> IdList a -> IdList a
cons x xs = snd (cons' (const x) xs)

cons' :: (Id -> a) -> IdList a -> (Id, IdList a)
cons' f (IdList ixs newId@(Id i)) = (newId, IdList ((Id i, f newId) : ixs) (Id (succ i)))

snoc :: a -> IdList a -> IdList a
snoc x xs = snd (snoc' (const x) xs)

snoc' :: (Id -> a) -> IdList a -> (Id, IdList a)
snoc' f (IdList ixs newId@(Id i)) = (newId, IdList (ixs ++ [(Id i, f newId)]) (Id (succ i)))

contents :: ([(Id, a)] -> [(Id, b)]) -> IdList a -> IdList b
contents f (IdList ixs i) = IdList (f ixs) i

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
  let (i, list') = cons' (const x) list
  puts label list'
  return i

snocM :: MonadState s m => (s :-> IdList a) -> a -> m Id
snocM label x = do
  list <- gets label
  let (i, list') = snoc' (const x) list
  puts label list'
  return i

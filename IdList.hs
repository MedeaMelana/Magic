module IdList where

import Prelude hiding (filter)
import qualified Prelude

type Id = Int

data IdList a = IdList [(Id, a)] Id

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

remove :: Id -> IdList a -> IdList a
remove i = contents (Prelude.filter (\(i', _) -> i /= i'))

cons :: a -> IdList a -> IdList a
cons x (IdList ixs i) = IdList ((i, x) : ixs) (succ i)

contents :: ([(Id, a)] -> [(Id, a)]) -> IdList a -> IdList a
contents f (IdList ixs i) = IdList (f ixs) i

toList :: IdList a -> [(Id, a)]
toList (IdList ixs _) = ixs

filter :: (a -> Bool) -> IdList a -> [(Id, a)]
filter f = Prelude.filter (f . snd) . toList

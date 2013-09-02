{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Magic.Some (Some(..), Show1(..)) where

import Data.Type.Equality

data Some f where Some :: f a -> Some f

class Show1 f where
  show1 :: f a -> String

instance Show1 f => Show (Some f) where
  show (Some x) = show1 x

instance EqT f => Eq (Some f) where
  Some x == Some y =
    case eqT x y of
      Just Refl -> True
      Nothing   -> False

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module TargetList where

import Prelude hiding (length)

import Control.Applicative

data Target = TargetCreature | TargetPlayer

data TargetList t a where
  Nil :: a -> TargetList t a
  Snoc :: TargetList t (Target -> a) -> t -> TargetList t a

instance Functor (TargetList t) where
  fmap f (Nil x) = Nil (f x)
  fmap f (Snoc xs t) = Snoc (fmap (f .) xs) t

instance Applicative (TargetList t) where
  pure = Nil
  xs <*> Nil b     = fmap ($ b) xs
  xs <*> Snoc ys t = Snoc ((.) <$> xs <*> ys) t

collect :: TargetList Target a -> ([Target], a)
collect (Nil x) = ([], x)
collect (Snoc xs t) = (t:ts, f t)
  where
    (ts, f) = collect xs

length :: TargetList t a -> Int
length (Nil _) = 0
length (Snoc xs _) = succ (length xs)

targetCreature :: TargetList () Target
targetCreature = Snoc (Nil id) ()

targetPlayer :: TargetList () Target
targetPlayer = Snoc (Nil id) ()

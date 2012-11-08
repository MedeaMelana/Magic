{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module TargetList where

import Prelude hiding (length)

import Control.Applicative


-- data Target

data Target = TargetCreature Int | TargetPlayer Int
  deriving (Eq, Show, Ord)

isTargetCreature :: Target -> Bool
isTargetCreature (TargetCreature _) = True
isTargetCreature (TargetPlayer _)   = False

isTargetPlayer :: Target -> Bool
isTargetPlayer (TargetCreature _) = False
isTargetPlayer (TargetPlayer _)   = True


-- data TargetList

-- Tyarg t is either () for a list of fixed length with placeholders
-- or Target for a list of fixed length with actual Target elements
--
-- Important properties of a target list are:
-- * It has a placeholder version and a filled version
-- * Its length does not depend on its values (it is not a monad)
-- * If filled, its element targets can be inspected and enumerated
-- * Single target elements may be replaced by other targets, after which the result type (a) and intermediate predicates can be reevaluated according to the list's new contents
-- * Predicates in Test nodes may read preceding Target elements; this allows for early filtering when interactively replacing placeholders by actual values (see askTargets below)
data TargetList t a where
  Nil  :: a -> TargetList t a
  Snoc :: TargetList t (Target -> a) -> t -> TargetList t a
  Test :: (x -> a) -> (x -> Bool) -> TargetList t x -> TargetList t a

instance Functor (TargetList t) where
  fmap f (Nil x)        = Nil (f x)
  fmap f (Snoc xs t)    = Snoc (fmap (f .) xs) t
  fmap f (Test g ok xs) = Test (f . g) ok xs

instance Applicative (TargetList t) where
  pure = Nil
  xs <*> Nil b     = fmap ($ b) xs
  xs <*> Snoc ys t = Snoc ((.) <$> xs <*> ys) t
  xs <*> Test f ok ys = Test fst snd ((\g x -> (g (f x), ok x)) <$> xs <*> ys)
  -- xs <*> Test f ok ys :: TL b
  -- xs :: TL (a -> b)
  -- Test f ok ys :: TL a
  -- f :: x -> a
  -- ok :: x -> Bool
  -- ys :: TL x
  -- gevraagd: Test f' ok' zs :: TL b
  -- misschien: x' ~ (a -> b, x)
  -- f' :: x' -> b
  -- ok' :: x' -> Bool
  -- zs :: TL x'

instance Show a => Show (TargetList Target a) where
  show = show . snd . evaluate


-- utility functions that consume TargetLists

evaluate :: TargetList Target a -> ([Target], a)
evaluate (Nil x)       = ([], x)
evaluate (Snoc xs t)   = (ts ++ [t], f t) where (ts, f) = evaluate xs
evaluate (Test f _ xs) = (ts,        f x) where (ts, x) = evaluate xs

length :: TargetList t a -> Int
length (Nil _)       = 0
length (Snoc xs _)   = succ (length xs)
length (Test _ _ xs) = length xs


-- TargetList producers

singleTarget :: TargetList () Target
singleTarget = Snoc (Nil id) ()

targetCreature :: TargetList () Target
targetCreature = singleTarget <?> isTargetCreature

targetPlayer :: TargetList () Target
targetPlayer = singleTarget <?> isTargetPlayer

-- http://magiccards.info/pc2/en/39.html
arcTrailTargets :: TargetList () (Target, Target)
arcTrailTargets = (,) <$> singleTarget <*> singleTarget <?> uncurry (/=)

infixl 4 <?>
(<?>) :: TargetList t a -> (a -> Bool) -> TargetList t a
xs <?> ok = Test id ok xs


-- Interactively replace placeholders () by actual targets
-- The first argument is all available targets in the world
askTargets :: forall m a. Monad m => ([Target] -> m Target) -> [Target] -> TargetList () a -> m (TargetList Target a)
askTargets choose = askTargets' (const True)
  where
    askTargets' :: forall a. (a -> Bool) -> [Target] -> TargetList () a -> m (TargetList Target a)
    askTargets' ok ts scheme =
      case scheme of
        Nil x -> return (Nil x)
        Snoc xs () -> do
          xs' <- askTargets choose ts xs
          let (_, f) = evaluate xs'
          let eligibleTargets = filter (ok . f) ts
          chosen <- choose eligibleTargets
          return (Snoc xs' chosen)
        Test f ok' scheme' -> do
          z <- askTargets' (\x -> ok (f x) && ok' x) ts scheme'
          return (f <$> z)

chooseIO :: [Target] -> IO Target
chooseIO eligibleTargets = do
  putStr ("Possible targets: " ++ show eligibleTargets ++ " !! ")
  index <- read <$> getLine
  return (eligibleTargets !! index)

test0 :: IO (Target, Target)
test0 = snd . evaluate <$> askTargets chooseIO [TargetCreature 0, TargetCreature 1] arcTrailTargets

test1 :: [(Target, Target)]
test1 = snd . evaluate <$> askTargets id [TargetCreature 0, TargetCreature 1] arcTrailTargets

-- testing travis
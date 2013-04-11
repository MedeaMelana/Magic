{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Magic.Target (
    -- * Types
    TargetList(..), Target(..),

    -- * Producing target lists
    target, (<?>),

    -- * Compiling target lists
    evaluateTargetList, askMagicTargets
  ) where

import qualified Magic.IdList as IdList
import Magic.Core
import Magic.Types

import Control.Applicative
import Control.Monad (forM, filterM)
import Data.Label.PureM (asks)


evaluateTargetList :: TargetList Target a -> ([Target], Maybe a)
evaluateTargetList (Nil x) = ([], Just x)
evaluateTargetList (Snoc xs cast t) = (ts ++ [t], mf <*> cast t)
  where (ts, mf) = evaluateTargetList xs
evaluateTargetList (Test f _ xs) = (ts, f <$> mx)
  where (ts, mx) = evaluateTargetList xs

target :: (Target -> Maybe a) -> TargetList () a
target f = Snoc (Nil id) f ()

infixl 4 <?>
(<?>) :: TargetList t a -> (a -> View Bool) -> TargetList t a
xs <?> ok = Test id ok xs

askTargets :: forall a. ([Target] -> Magic Target) -> [Target] -> TargetList () a -> Magic (TargetList Target a, a)
askTargets choose = askTargets' (const (return True))
  where
    askTargets' :: forall b. (b -> View Bool) -> [Target] -> TargetList () b -> Magic (TargetList Target b, b)
    askTargets' ok ts scheme =
      case scheme of
        Nil x -> return (Nil x, x)
        Snoc tsf cast () -> do
          (tsf', f) <- askTargets' (const (return True)) ts tsf
          eligibleTargets <- view $ flip filterM ts $ \t -> do
            case cast t of
              Just y -> ok (f y)
              Nothing -> return False
          chosen <- choose eligibleTargets
          let Just x = cast chosen
          return (Snoc tsf' cast chosen, f x)
        Test f ok' tsx -> do
          (tsx', x) <- askTargets' (\x -> (&&) <$> ok (f x) <*> ok' x) ts tsx
          return (f <$> tsx', f x)

askMagicTargets :: PlayerRef -> TargetList () a -> Magic (TargetList Target a)
askMagicTargets p ts = do
  ats <- allTargets
  fst <$> askTargets (askQuestion p . AskTarget) ats ts

allTargets :: Magic [Target]
allTargets = do
  ps <- IdList.ids <$> view (asks players)
  let zrs = [Exile, Battlefield, Stack, Command] ++
            [ z p | z <- [Library, Hand, Graveyard], p <- ps ]
  oss <- forM zrs $ \zr -> do
    os <- IdList.ids <$> view (asks (compileZoneRef zr))
    return (map (\o -> (zr, o)) os)
  return (map TargetPlayer ps ++ map TargetObject (concat oss))


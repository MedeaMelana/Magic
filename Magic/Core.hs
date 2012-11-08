{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Magic.Core
  --( compileZoneRef
  --, evaluateTargetList, singleTarget, (<?>), askMagicTargets, allTargets, allObjects
  --, module Types
  --)
  where

import Magic.IdList (IdList)
import qualified Magic.IdList as IdList
import Magic.Labels
import Magic.Types

import Control.Applicative
import qualified Control.Monad.Operational as Operational
import Control.Monad (forM, filterM)
import Control.Monad.Reader (runReaderT)
import qualified Control.Monad.State as State
import Control.Monad.Trans (lift)
import Data.Label.Pure ((:->))
import Data.Label.PureM (asks)


compileZoneRef :: ZoneRef -> World :-> IdList Object
compileZoneRef z =
  case z of
    Library p   -> players .^ listEl p .^ library
    Hand p      -> players .^ listEl p .^ hand
    Battlefield -> battlefield
    Graveyard p -> players .^ listEl p .^ graveyard
    Stack       -> stack
    Exile       -> exile
    Command     -> command

evaluateTargetList :: TargetList Target a -> ([Target], a)
evaluateTargetList (Nil x)       = ([], x)
evaluateTargetList (Snoc xs t)   = (ts ++ [t], f t) where (ts, f) = evaluateTargetList xs
evaluateTargetList (Test f _ xs) = (ts,        f x) where (ts, x) = evaluateTargetList xs

singleTarget :: TargetList () Target
singleTarget = Snoc (Nil id) ()

infixl 4 <?>
(<?>) :: TargetList t a -> (a -> View Bool) -> TargetList t a
xs <?> ok = Test id ok xs

askTargets :: forall a. ([Target] -> Magic Target) -> [Target] -> TargetList () a -> Magic (TargetList Target a)
askTargets choose = askTargets' (const (return True))
  where
    askTargets' :: forall b. (b -> View Bool) -> [Target] -> TargetList () b -> Magic (TargetList Target b)
    askTargets' ok ts scheme =
      case scheme of
        Nil x -> return (Nil x)
        Snoc xs () -> do
          xs' <- askTargets choose ts xs
          let (_, f) = evaluateTargetList xs'
          eligibleTargets <- view (filterM (ok . f) ts)
          chosen <- choose eligibleTargets
          return (Snoc xs' chosen)
        Test f ok' scheme' -> do
          z <- askTargets' (\x -> (&&) <$> ok (f x) <*> ok' x) ts scheme'
          return (f <$> z)

askMagicTargets :: PlayerRef -> TargetList () a -> Magic (TargetList Target a)
askMagicTargets p ts = do
  ats <- allTargets
  askTargets (lift . Operational.singleton . AskTarget p) ats ts

allTargets :: Magic [Target]
allTargets = do
  ps <- IdList.ids <$> asks players
  let zrs = [Exile, Battlefield, Stack, Command] ++
            [ z p | z <- [Library, Hand, Graveyard], p <- ps ]
  oss <- forM zrs $ \zr -> do
    os <- IdList.ids <$> asks (compileZoneRef zr)
    return (map (\o -> (zr, o)) os)
  return (map TargetPlayer ps ++ map TargetObject (concat oss))

allObjects :: Magic [(ObjectRef, Object)]
allObjects = do
  ps <- IdList.ids <$> asks players
  let zrs = [Exile, Battlefield, Stack, Command] ++
            [ z p | z <- [Library, Hand, Graveyard], p <- ps ]
  fmap concat $ forM zrs $ \zr -> do
    ios <- IdList.toList <$> asks (compileZoneRef zr)
    return (map (\(i,o) -> ((zr,i),o)) ios)

liftQuestion :: Ask a -> Engine a
liftQuestion = lift . lift . Operational.singleton

executeMagic :: Magic a -> Engine a
executeMagic m = State.get >>= lift . lift . runReaderT m

object :: ObjectRef -> World :-> Object
object (zoneRef, i) = compileZoneRef zoneRef .^ listEl i

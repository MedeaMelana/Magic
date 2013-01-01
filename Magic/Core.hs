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
import Control.Monad (forM)
import Control.Monad.Reader (runReaderT, ask)
import qualified Control.Monad.State as State
import Control.Monad.Trans (lift)
import Data.Label.Pure ((:->))
import Data.Label.PureM (asks)
import Data.Text (Text)


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

allObjects :: Magic [(ObjectRef, Object)]
allObjects = do
  ps <- IdList.ids <$> asks players
  let zrs = [Exile, Battlefield, Stack, Command] ++
            [ z p | z <- [Library, Hand, Graveyard], p <- ps ]
  fmap concat $ forM zrs $ \zr -> do
    ios <- IdList.toList <$> asks (compileZoneRef zr)
    return (map (\(i,o) -> ((zr,i),o)) ios)

liftQuestion :: PlayerRef -> Question a -> Magic a
liftQuestion p q = do
  world <- ask
  lift (Operational.singleton (AskQuestion p world q))

debug :: Text -> Magic ()
debug t = lift (Operational.singleton (Debug t))

debugEngine :: Text -> Engine ()
debugEngine t = executeMagic (lift (Operational.singleton (Debug t)))

liftEngineQuestion :: PlayerRef -> Question a -> Engine a
liftEngineQuestion p q = executeMagic (liftQuestion p q)

executeMagic :: Magic a -> Engine a
executeMagic m = State.get >>= lift . lift . runReaderT m

object :: ObjectRef -> World :-> Object
object (zoneRef, i) = compileZoneRef zoneRef .^ listEl i

player :: PlayerRef -> World :-> Player
player i = players .^ listEl i

isStackEmpty :: View Bool
isStackEmpty = IdList.null <$> asks stack

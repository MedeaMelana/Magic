{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Magic.Core
  ( compileZoneRef, allObjects, askQuestion, debug, object, player, isStackEmpty )
  where

import Magic.IdList (IdList)
import qualified Magic.IdList as IdList
import Magic.Labels
import Magic.Types

import Control.Applicative
import Control.Monad (forM)
import Control.Monad.Reader (ask)
import Control.Monad.Operational (singleton)
import Data.Label.Pure ((:->))
import Data.Label.PureM (asks)
import Data.Text (Text)
import Prelude hiding (interact)


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

allObjects :: View [(ObjectRef, Object)]
allObjects = do
  ps <- IdList.ids <$> asks players
  let zrs = [Exile, Battlefield, Stack, Command] ++
            [ z p | z <- [Library, Hand, Graveyard], p <- ps ]
  fmap concat $ forM zrs $ \zr -> do
    ios <- IdList.toList <$> asks (compileZoneRef zr)
    return (map (\(i,o) -> ((zr,i),o)) ios)

askQuestion :: (MonadInteract m, MonadView m) => PlayerRef -> Question a -> m a
askQuestion p q = do
  world <- view ask
  interact (singleton (AskQuestion p world q))

debug :: MonadInteract m => Text -> m ()
debug t = interact (singleton (Debug t))

object :: ObjectRef -> World :-> Object
object (zoneRef, i) = compileZoneRef zoneRef .^ listEl i

player :: PlayerRef -> World :-> Player
player i = players .^ listEl i

isStackEmpty :: View Bool
isStackEmpty = IdList.null <$> asks stack

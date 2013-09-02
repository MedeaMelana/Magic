{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Magic.Core
  ( compileZoneRef, allObjects, askQuestion, debug, object, objectBase, objectPart, anyObject, player, isStackEmpty )
  where

import Magic.Some (Some(..))
import Magic.IdList (IdList)
import qualified Magic.IdList as IdList
import Magic.Labels
import Magic.Types

import Control.Applicative
import Control.Monad.Reader (ask)
import Control.Monad.Operational (singleton)
import Data.Label (lens)
import Data.Label.Pure ((:->))
import Data.Label.PureM (asks)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Traversable (for)
import Prelude hiding (interact)


compileZoneRef :: ZoneRef ty -> World :-> IdList (ObjectOfType ty)
compileZoneRef z =
  case z of
    Library p   -> players .^ listEl p .^ library
    Hand p      -> players .^ listEl p .^ hand
    Battlefield -> battlefield
    Graveyard p -> players .^ listEl p .^ graveyard
    Stack       -> stack
    Exile       -> exile
    Command     -> command

allObjects :: View [(SomeObjectRef, Object)]
allObjects = do
    ips <- IdList.toList <$> asks players
    sharedObjects <> (concat <$> for ips objectsForPlayer)
  where
    sharedObjects =
        (map (\(i, Permanent p) -> ((Some Battlefield, i), p)) . IdList.toList <$> asks battlefield)
      <>
        (map (\(i, StackItem s) -> ((Some Stack, i), s)) . IdList.toList <$> asks stack)
      <>
        (map (\(i, CardObject c) -> ((Some Exile, i), c)) . IdList.toList <$> asks exile)
      <>
        (map (\(i, CardObject c) -> ((Some Command, i), c)) . IdList.toList <$> asks command)
    objectsForPlayer (ip, p) = return $
      [ ((Some (Library ip), i), c) | (i, CardObject c) <- IdList.toList (_library p) ]
      <>
      [ ((Some (Hand ip), i), c) | (i, CardObject c) <- IdList.toList (_hand p) ]
      <>
      [ ((Some (Graveyard ip), i), c) | (i, CardObject c) <- IdList.toList (_graveyard p) ]

askQuestion :: (MonadInteract m, MonadView m) => PlayerRef -> Question a -> m a
askQuestion p q = do
  world <- view ask
  interact (singleton (AskQuestion p world q))

debug :: MonadInteract m => Text -> m ()
debug t = interact (singleton (Debug t))

object :: ObjectRef ty -> World :-> ObjectOfType ty
object (zoneRef, i) = compileZoneRef zoneRef .^ listEl i

objectBase :: SomeObjectRef -> World :-> Object
objectBase (Some zr, i) = compileZoneRef zr .^ listEl i .^ objectPart

objectPart :: ObjectOfType ty :-> Object
objectPart = lens getObjectPart setObjectPart
  where
    getObjectPart :: ObjectOfType ty -> Object
    getObjectPart (CardObject o) = o
    getObjectPart (Permanent o) = o
    getObjectPart (StackItem o) = o

    setObjectPart :: Object -> ObjectOfType ty -> ObjectOfType ty
    setObjectPart o (CardObject _) = CardObject o
    setObjectPart o (Permanent _)  = Permanent o
    setObjectPart o (StackItem _)  = StackItem o

anyObject :: SomeObjectRef -> World -> Some ObjectOfType
anyObject = undefined

player :: PlayerRef -> World :-> Player
player i = players .^ listEl i

isStackEmpty :: View Bool
isStackEmpty = IdList.null <$> asks stack

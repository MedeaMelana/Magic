module Magic.Events (
    -- * Types
    OneShotEffect(..), SimpleOneShotEffect(..), Event(..),

    -- * Constructing specific one-shot effects
    willMoveToGraveyard, willMoveToBattlefield, willMoveToStack,
  ) where

import Magic.Core
import Magic.IdList (Id)
import Magic.Types

import Data.Label.Pure (get, set)
import Data.Label.PureM (asks)
import Prelude hiding (interact)



-- CONSTRUCTING SPECIFIC ONE-SHOT EFFECTS


-- | Effect that moves the specified object on the battlefield to its owner's graveyard.
willMoveToGraveyard :: Id -> Object -> OneShotEffect
willMoveToGraveyard i o = WillMoveObject (Battlefield, i) (Graveyard (get owner o)) o

willMoveToBattlefield :: ObjectRef -> View OneShotEffect
willMoveToBattlefield r = do
  o <- asks (object r)
  let o' = o { _tapStatus = Just Untapped }
  return (WillMoveObject r Battlefield o')

willMoveToStack :: ObjectRef -> StackItem -> View OneShotEffect
willMoveToStack r si = do
  o <- asks (object r)
  return (WillMoveObject r Stack (set stackItem (Just si) o))



-- EXECUTING EFFECTS



{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Magic.Events (
    -- * Types
    OneShotEffect(..), SimpleOneShotEffect(..), Event(..),

    -- * Constructing specific one-shot effects
    willMoveToGraveyard, willMoveToStack, -- willMoveToBattlefield,
    willMoveToExile,
    shuffleIntoLibrary,
    searchCard,

    executeEffects, executeEffect, will,
    tick
  ) where

import Magic.Some (Some(..))
import Magic.Core
import Magic.Types
import qualified Magic.IdList as IdList

import Data.Function (on)
import Data.Functor (void)
import Data.List (nub, nubBy)
import Data.Traversable (for)
import Control.Applicative ((<$>))
import Control.Monad.Operational (singleton)
import Control.Monad.Trans (lift)
import Data.Label (get)
import Data.Label.Monadic (asks)
import Data.Maybe (catMaybes)
import Prelude hiding (interact, pred)



-- CONSTRUCTING SPECIFIC ONE-SHOT EFFECTS


-- | Effect that moves the specified object on the battlefield to its owner's graveyard.
willMoveToGraveyard :: ObjectRef TyPermanent -> Object -> OneShotEffect
willMoveToGraveyard (Battlefield, i) o = WillMoveObject (Just (Some Battlefield, i)) (Graveyard (get owner o)) (CardObject o)

-- | Effect that exiles the specified object
willMoveToExile :: ObjectRef TyPermanent -> Object -> OneShotEffect
willMoveToExile (zone, i) o = WillMoveObject (Just (Some zone, i)) Exile (CardObject o)

--willMoveToBattlefield :: SomeObjectRef -> View OneShotEffect
--willMoveToBattlefield r = do
--  o <- asks (object r)
--  let o' = o { _tapStatus = Just Untapped }
--  return (WillMoveObject (Just r) Battlefield o')

willMoveToStack :: SomeObjectRef -> StackItem -> View OneShotEffect
willMoveToStack r si = do
  o <- asks (objectBase r)
  return (WillMoveObject (Just r) Stack (StackItem o si))

-- | Shuffle a bunch of cards into their owners' libraries. Object references
-- that can't be resolved are ignored. If the cards are owned by multiple
-- players, each of those players' libraries is shuffled.
--
-- The libraries of the separately specified players are always shuffled,
-- regardless of whether any of the cards were owned by those players. Use this
-- to implement effects that fall under rule [701.16e], where generic sets of
-- cards (e.g. "your graveyard") are shuffled into their owner's library.
--
-- Each player's library is shuffled at most once.
shuffleIntoLibrary :: [SomeObjectRef] -> [PlayerRef] -> Magic [Event]
shuffleIntoLibrary rs ps = do
  -- [701.16c] Resolve references
  ros <- fmap (nubBy ((==) `on` fst) . catMaybes) $ view $ for rs $ \r ->
    fmap (r, ) <$> viewSomeObject r
  moves <- executeEffects
    [ WillMoveObject (Just r) (Library (get owner o)) (CardObject o)
    | (r, o) <- ros ]
  -- [701.16d] Check which libraries actually received cards
  let affectedLibraries =
        [ p | DidMoveObject _ (Some (Library p), _) <- moves ]
  (moves ++) <$> executeEffects
    [ Will (ShuffleLibrary p)
    | p <- nub (ps ++ affectedLibraries)
    ]

-- | Asks the given player to search in the give zone for objects satisfying
-- a certain predicate.
searchCard :: PlayerRef -> ZoneRef ty -> (Object -> Bool) -> Magic (Maybe IdList.Id)
searchCard p zone pred = do
    ids <- IdList.toList <$> view (asks (compileZoneRef zone))
    let eligibleTargets = ids >>= (\(i, o) -> let obj = get objectPart o
                                              in [i | pred obj])
    askQuestion p (AskSearch zone eligibleTargets)


-- EXECUTING EFFECTS


executeEffects :: [OneShotEffect] -> Magic [Event]
executeEffects = Magic . lift . singleton . ExecuteEffects

executeEffect :: OneShotEffect -> Magic [Event]
executeEffect = executeEffects . (: [])

-- Execute a single simple effect in a single event, discarding the result.
will :: SimpleOneShotEffect -> Magic ()
will eff = void (executeEffect (Will eff))

tick :: Magic Timestamp
tick = Magic $ lift $ singleton Tick

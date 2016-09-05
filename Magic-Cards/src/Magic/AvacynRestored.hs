{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Magic.AvacynRestored where

import Magic

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Monad (void)
import Data.Boolean ((||*), false)
import Data.Label.Monadic (asks, (=:))
import Data.Monoid ((<>))
import Prelude hiding ((.))



misthollowGriffin :: Card
misthollowGriffin = mkCard $ do
    name =: Just "Misthollow Griffin"
    types =: creatureTypes [Griffin]
    pt =: Just (3, 3)
    staticKeywordAbilities =+ [Flying]
    play =: Just playObject
      { available = availableFromHand ||* availableFromExile
      , manaCost  = Just (generic 2 <> blue 2)
      }
  where
    availableFromExile :: Contextual (View Bool)
    availableFromExile rSelf you =
      case rSelf of
        (Some Exile, _) -> (== you) <$> view (asks (owner . objectBase rSelf))
        _          -> false

bloodArtist :: Card
bloodArtist = mkCard $ do
    name =: Just "Blood Artist"
    types =: creatureTypes [Vampire]
    pt =: Just (0, 1)
    play =: Just playObject { manaCost = Just (generic 1 <> black 1) }
    triggeredAbilities =: ifSelfWasOrIsOnBattlefield trigger
  where
    trigger :: TriggeredAbilities
    trigger events _rSelf you = return [ createTriggerObject you
      | DidMoveObject (Just (Some Battlefield, _)) (Some (Graveyard _), _) <- events ]

    createTriggerObject :: PlayerRef -> Magic ()
    createTriggerObject you = do
      ts <- askTarget you targetPlayer
      mkTargetTrigger you ts $ \p ->
        void $ executeEffects [Will (LoseLife p 1), Will (GainLife you 1)]

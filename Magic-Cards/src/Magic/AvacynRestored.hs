{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Magic.AvacynRestored where

import Magic
import Magic.Labels ((.^))
import Control.Applicative (pure, (<$>))
import Control.Monad (void)
import Data.Boolean ((||*), false)
import Data.Label (get, modify)
import Data.Label.Monadic (asks, (=:))

misthollowGriffin :: Card
misthollowGriffin = mkCard $ do
    name =: Just "Misthollow Griffin"
    types =: creatureTypes [Griffin]
    pt =: Just (3, 3)
    staticKeywordAbilities =: [Flying]
    play =: Just playObject
      { available = availableFromHand ||* availableFromExile
      , manaCost  = Just [Nothing, Nothing, Just Blue, Just Blue]
      }
  where
    availableFromExile :: Contextual (View Bool)
    availableFromExile rSelf you =
      case rSelf of
        (Some Exile, _) -> (== you) <$> view (asks (objectBase rSelf .^ owner))
        _          -> false



bloodArtist :: Card
bloodArtist = mkCard $ do
    name =: Just "Blood Artist"
    types =: creatureTypes [Vampire]
    pt =: Just (0, 1)
    play =: Just playObject { manaCost = Just [Nothing, Just Black] }
    triggeredAbilities =: ifSelfWasOrIsOnBattlefield trigger
  where
    trigger :: TriggeredAbilities
    trigger events _rSelf you = return [ createTriggerObject you
      | DidMoveObject (Just (Some Battlefield, _)) (Some (Graveyard _), _) <- events ]

    createTriggerObject :: PlayerRef -> Magic ()
    createTriggerObject you = do
      ts <- askMagicTargets you targetPlayer
      mkTriggerObject you ts $ \p _source ->
        void $ executeEffects [Will (LoseLife p 1), Will (GainLife you 1)]

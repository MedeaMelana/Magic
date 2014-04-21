{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Magic.AvacynRestored where

import Magic
import Control.Applicative (pure)
import Control.Monad (void)
import Data.Label (get, modify)
import Data.Label.Monadic (asks, (=:))

misthollowGriffin :: Card
misthollowGriffin = mkCard $ do
    name =: Just "Misthollow Griffin"
    types =: creatureTypes [Griffin]
    pt =: Just (3, 3)
    staticKeywordAbilities =: [Flying]
    play =: Just Activation
      { available     = \rSelf you -> do
          case rSelf of
            (Some (Hand you'), _) | you == you' -> checkTiming rSelf you
            (Some Exile, _)                     -> checkTiming rSelf you
            _ -> return False
      , manaCost      = [Nothing, Nothing, Just Blue, Just Blue]
      , effect        = playPermanentEffect
      }
  where
    playPermanentEffect :: Contextual (Magic ())
    playPermanentEffect rSelf _ = void $
        view (willMoveToStack rSelf (pure resolvePermanent)) >>= executeEffect

    checkTiming :: Contextual (View Bool)
    checkTiming rSelf you = do
      self <- asks (objectBase rSelf)
      if Flash `elem` get staticKeywordAbilities self
        then return True
        else sorcerySpeed rSelf you

    resolvePermanent _source = return ()



bloodArtist :: Card
bloodArtist = mkCard $ do
    name =: Just "Blood Artist"
    types =: creatureTypes [Vampire]
    pt =: Just (0, 1)
    play =: Just (playPermanent [Nothing, Just Black])
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

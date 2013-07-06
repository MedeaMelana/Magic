{-# LANGUAGE OverloadedStrings #-}

module Magic.AvacynRestored where

import Magic
import Control.Monad (void)
import Data.Label.PureM ((=:))

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
      | DidMoveObject (Just (Battlefield, _)) (Graveyard _, _) <- events ]

    createTriggerObject :: PlayerRef -> Magic ()
    createTriggerObject you = do
      ts <- askMagicTargets you targetPlayer
      mkTriggerObject you ts $ \p _source ->
        void $ executeEffects [Will (LoseLife p 1), Will (GainLife you 1)]

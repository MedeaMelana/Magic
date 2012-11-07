{-# LANGUAGE OverloadedStrings #-}

module M12 where

import Core
import Types
import Utils

import Control.Applicative
import Data.Label.PureM ((=:))


shock :: Card
shock = mkCard $ do
  name  =: Just "Shock"
  types =: instantType
  play  =: (Just $ \rSelf rActivator ->
    ClosedAbility
      { _available =
          case rSelf of
            (Hand rp, _) -> return (rp == rActivator)
            _            -> return False
      , _manaCost = ManaCost [Red] 0
      , _additionalCosts = []
      , _effect = StackingAction (shockEffect rSelf rActivator)
      })

shockEffect :: ObjectRef -> PlayerRef -> Magic StackItem
shockEffect rSelf rActivator = do
  -- TODO check for hexproof
  -- TODO check for protection
  -- TODO realise rSelf is sometimes in hand, sometimes on the stack
  let ok t = case t of
              TargetObject (Battlefield, _) -> return True
              TargetPlayer _                -> return True
              _                             -> return False
  ts <- askMagicTargets rActivator (singleTarget <?> ok)
  let f t = case t of
              TargetObject or -> return [Will (DamageObject rSelf or 2 False True)]
              TargetPlayer pr -> return [Will (DamagePlayer rSelf pr 2 False True)]
  return (f <$> ts)

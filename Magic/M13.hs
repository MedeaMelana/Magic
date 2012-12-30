{-# LANGUAGE OverloadedStrings #-}

module Magic.M13 where

import Magic.Types
import Magic.ObjectTypes
import Magic.Target
import Magic.Utils
import Magic.Core
import Magic.Events

import Control.Applicative
import Data.Label.PureM ((=:), asks)


searingSpear :: Card
searingSpear = mkCard $ do
  name  =: Just "Searing Spear"
  types =: instantType
  play  =: (Just $ \rSelf rActivator ->
    ClosedAbility
      { _available =
          case rSelf of
            (Hand rp, _) -> return (rp == rActivator)
            _            -> return False
      , _manaCost = ManaCost [Red] 1
      , _additionalCosts = []
      , _effect = searingSpearEffect rSelf rActivator
      })

searingSpearEffect :: ObjectRef -> PlayerRef -> Magic [OneShotEffect]
searingSpearEffect rSelf rActivator = do
  let ok t = case t of
              TargetObject r@(Battlefield, _) -> hasTypes creatureType <$> asks (object r)
              TargetPlayer _                -> return True
              _                             -> return False
  ts <- askMagicTargets rActivator (singleTarget <?> ok)
  let f t = case t of
              TargetObject r -> return [Will (DamageObject rSelf r 3 False True)]
              TargetPlayer r -> return [Will (DamagePlayer rSelf r 3 False True)]
  (: []) <$> view (willMoveToStack rSelf (f <$> ts))

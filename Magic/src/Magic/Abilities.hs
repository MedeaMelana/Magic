{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Magic.Abilities (
    -- * Ability types
    Contextual,
    ActivatedAbility(..), TapCost(..),
    StackItem, ManaPool,
    StaticKeywordAbility(..),
    ReplacementEffect, TriggeredAbilities,
    PriorityAction(..), PayManaAction(..),

    -- * Cast speed
    instantSpeed, sorcerySpeed,

    -- * Play Abilities
    playPermanent, playAura, stackTargetlessEffect,

    -- * Constructing triggers
    mkTriggerObject, mkTargetlessTriggerObject, onSelfETB,
    ifSelfWasOrIsOnBattlefield,

    -- * Constructing replacement effects
    etbWithLoyaltyCounters
  ) where

import Magic.Core
import Magic.Events
import Magic.Labels
import Magic.Predicates
import Magic.Some
import Magic.Target
import Magic.Types
import Magic.Utils (gand, emptyObject)

import Control.Applicative ((<$>), pure)
import Control.Monad (void)

import Data.Boolean ((&&*))
import Data.Label.Pure (get, modify)
import Data.Label.PureM (asks)
import Data.Monoid (mempty)



-- CAST SPEED


instantSpeed :: Contextual (View Bool)
instantSpeed rSelf rActivator =
  case rSelf of
    (Some (Hand rp), _) -> return (rp == rActivator)
    _                   -> return False

sorcerySpeed :: Contextual (View Bool)
sorcerySpeed rSelf rp = instantSpeed rSelf rp &&* myMainPhase &&* isStackEmpty
  where
    myMainPhase = do
      ap <- asks activePlayer
      as <- asks activeStep
      return (ap == rp && as == MainPhase)



-- PLAY ABILITIES


-- | Play a nonland, non-aura permanent.
playPermanent :: ManaPool -> ActivatedAbility
playPermanent mc =
  ActivatedAbility
    { available     = \rSelf rActivator -> do
        self <- asks (objectBase rSelf)
        if Flash `elem` get staticKeywordAbilities self
          then instantSpeed rSelf rActivator
          else sorcerySpeed rSelf rActivator
    , manaCost      = mc
    , tapCost       = NoTapCost
    , effect        = playPermanentEffect
    , isManaAbility = False
    }
  where
    playPermanentEffect :: Contextual (Magic ())
    playPermanentEffect rSelf _ = void $
        view (willMoveToStack rSelf (pure resolvePermanent)) >>= executeEffect

    resolvePermanent _source = return ()

playAura :: ManaPool -> ActivatedAbility
playAura mc =
  ActivatedAbility
    { available     = \rSelf rActivator -> do
        self <- asks (objectBase rSelf)
        if Flash `elem` get staticKeywordAbilities self
          then instantSpeed rSelf rActivator
          else sorcerySpeed rSelf rActivator
    , manaCost      = mc
    , tapCost       = NoTapCost
    , effect        = playAuraEffect
    , isManaAbility = False
    }
  where
    playAuraEffect :: Contextual (Magic ())
    playAuraEffect rSelf p = do
      aura <- view (asks (objectBase rSelf))  -- TODO Reevaluate rSelf on the stack?
      let ok r = collectEnchantPredicate aura <$>
                  asks (object r .^ objectPart)
      ts <- askMagicTargets p (target permanent <?> ok)
      let f :: ObjectRef TyPermanent -> ObjectRef TyStackItem -> Magic ()
          f (Battlefield, i) rStackSelf@(Stack, iSelf) = do
            self <- view (asks (object rStackSelf .^ objectPart))
            void $ executeEffect (WillMoveObject (Just (Some Stack, iSelf)) Battlefield (Permanent self Untapped 0 False (Just (Some Battlefield, i))))

      void $ view (willMoveToStack rSelf (f <$> ts)) >>= executeEffect

collectEnchantPredicate :: Object -> Object -> Bool
collectEnchantPredicate aura enchanted = gand
  [ hasTypes tys enchanted
  | EnchantPermanent tys <- get staticKeywordAbilities aura ]

stackTargetlessEffect :: SomeObjectRef -> (ObjectRef TyStackItem -> Magic ()) -> Magic ()
stackTargetlessEffect rSelf item = do
  eff <- view (willMoveToStack rSelf (pure item))
  void $ executeEffect eff




-- CONSTRUCTING TRIGGERS


-- | Creates a trigger on the stack under the control of the specified player.
-- The function is applied to the return value of the specified 'TargetList'
-- and put on the stack as a 'StackItem'.
mkTriggerObject :: PlayerRef -> TargetList EntityRef a ->
  (a -> ObjectRef TyStackItem -> Magic()) -> Magic ()
mkTriggerObject p ts f = do
  t <- tick
  void $ executeEffect $ WillMoveObject Nothing Stack $
    StackItem (emptyObject t p) (f <$> ts)


-- | Creates a trigger on the stack under the control of the specified player.
-- The specified program is wrapped in an empty 'TargetList' and passed to
-- 'mkTriggerObject'.
mkTargetlessTriggerObject :: PlayerRef -> (ObjectRef TyStackItem -> Magic()) -> Magic ()
mkTargetlessTriggerObject p f = mkTriggerObject p (pure ()) (const f)


-- | Trigger whenever the source object enters the battlefield, executing the
-- argument program.
onSelfETB :: Contextual (Magic ()) -> TriggeredAbilities
onSelfETB mkProgram events rSelf p = return [ mkProgram rSelf p
  | DidMoveObject _ rOther@(Some Battlefield, _) <- events, rSelf == rOther ]

-- | Modify a trigger to only fire when the source of the trigger is on the
-- battlefield now, or was before the events took place (i.e. one of the
-- events is the move of the source from the battlefield to another zone).
ifSelfWasOrIsOnBattlefield :: TriggeredAbilities -> TriggeredAbilities
ifSelfWasOrIsOnBattlefield f events rSelf you =
    if ok then f events rSelf you else mempty
  where
    ok = fst rSelf == Some Battlefield
      || not (null ([ () | DidMoveObject (Just (Some Battlefield, _)) newRef <- events, newRef == rSelf ] :: [()]))



-- CONSTRUCTING REPLACEMENT EFFECTS

etbWithLoyaltyCounters :: ReplacementEffect
etbWithLoyaltyCounters (WillMoveObject (Just fromRef) Battlefield perm) rSelf you
  | fromRef == rSelf =
      case get (objectPart .^ loyalty) perm of
        Just n -> Just $ return [WillMoveObject (Just fromRef) Battlefield (modify (objectPart .^ counters) (++ replicate n Loyalty) perm)]
        Nothing -> Nothing
etbWithLoyaltyCounters _ _ _ = Nothing

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Magic.Abilities (
    -- * Ability types
    Contextual,
    ActivatedAbility(..), Activation(..), TapCost(..), AbilityType(..),
    StackItem, ManaPool,
    StaticKeywordAbility(..),
    ReplacementEffect, TriggeredAbilities,
    PriorityAction(..), PayManaAction(..),

    -- * Timing restrictions
    -- | Timing restrictions that can be used for 'timing' fields in 'Activation's.
    instantSpeed, sorcerySpeed,

    -- * Availability
    -- | Checks that can be used for 'available' fields in 'Activation's.
    availableFromHand, availableFromBattlefield, availableFromGraveyard,

    -- * Playing objects
    playObject, playTiming, playObjectEffect,

    -- * Activated abilities
    loyaltyAbility, mkTargetAbility, mkAbility,
    tapAbility, tapAbilityWithCost, defaultActivation,

    -- * Creating effects on the stack
    stackSelf, stackTargetSelf,

    -- * Constructing triggers
    mkTargetTrigger, mkTrigger, onSelfETB,
    ifSelfWasOrIsOnBattlefield,

    -- * Constructing replacement effects
    etbWithLoyaltyCounters
  ) where

import Magic.Core
import Magic.Events
import Magic.ObjectTypes (instantType, sorceryType, auraType, landType, isObjectTypesSubsetOf)
import Magic.Predicates
import Magic.Some
import Magic.Target
import Magic.Types
import Magic.Utils (gand, emptyObject, countCountersOfType)

import Control.Applicative ((<$>), pure)
import Control.Category ((.))
import Control.Monad (void)

import Data.Boolean (true, false, (&&*))
import Data.Label (get, modify)
import Data.Label.Monadic (asks)
import Data.Monoid (mempty)
import Prelude hiding ((.))



-- TIMING RESTRICTIONS


-- | Timing restriction that checks whether an instant could be cast at this time.
instantSpeed :: Contextual (View Bool)
instantSpeed = true  -- TODO Check for Split second

-- | Timing restriction that checks whether a sorcery could be cast at this time.
sorcerySpeed :: Contextual (View Bool)
sorcerySpeed _rSelf rp = myMainPhase &&* isStackEmpty
  where
    myMainPhase = do
      ap <- asks activePlayer
      as <- asks activeStep
      return (ap == rp && as == MainPhase)



-- AVAILABILITY


-- | Checks that an activation's source is in the hand of the player who's trying to activate it, and that it's owned by that player.
availableFromHand :: Contextual (View Bool)
availableFromHand rSelf you =
  case rSelf of
    (Some (Hand you'), _) | you' == you ->
      (== you) <$> view (asks (controller . objectBase rSelf))
    _ -> false

-- | Checks whether an activation's source is on the battlefield and is controlled by the playing trying to activate it.
availableFromBattlefield :: Contextual (View Bool)
availableFromBattlefield rSelf you =
  case rSelf of
    (Some Battlefield, _) ->
      (== you) <$> view (asks (controller . objectBase rSelf))
    _ -> false


-- | Checks whether an activation's source is on the graveyard
availableFromGraveyard :: Contextual (View Bool)
availableFromGraveyard rSelf you =
  case rSelf of
    (Some (Graveyard you'), _) | you' == you ->
      (== you) <$> view (asks (controller . objectBase rSelf))
    _ -> false


-- PLAYING OBJECTS


-- | Default implementation for the play activation of an object, with timing
-- 'playTiming' and availability 'availableFromHand'. Most objects will want
-- to override the `manaCost`. Instants and sorceries need to override the
-- `effect` for it to do anything useful.
playObject :: Activation
playObject =
  Activation
    { timing    = playTiming
    , available = availableFromHand
    , manaCost  = Nothing
    , effect    = playObjectEffect
    }

-- | Default timing restriction for playing objects. Defers to `instantSpeed` if the object has `Flash` or is an `instantType`; otherwise defers to `sorcerySpeed`. Also checks restrictions for playing lands.
playTiming :: Contextual (View Bool)
playTiming rSelf you = do
    self <- asks (objectBase rSelf)
    let tys = get types self
    checkLandRestrictions tys &&* checkTimingRestrictions self
  where
    checkLandRestrictions :: ObjectTypes -> View Bool
    checkLandRestrictions tys
      | landType `isObjectTypesSubsetOf` tys = do
          ap <- asks activePlayer             -- [305.2]
          n  <- countLandsPlayedThisTurn you  -- [305.3]
          return (ap == you && n < 1)
      | otherwise = true

    countLandsPlayedThisTurn :: PlayerRef -> View Int
    countLandsPlayedThisTurn p =
        length . filter isPlayLand <$> asks turnHistory
      where
        isPlayLand (Did (PlayLand p' _))  = p == p'
        isPlayLand _                      = False

    checkTimingRestrictions :: Object -> View Bool
    checkTimingRestrictions self =
      if Flash `elem` get staticKeywordAbilities self ||
         instantType `isObjectTypesSubsetOf` (get types self)
        then instantSpeed rSelf you
        else sorcerySpeed rSelf you

-- | Default implementation for the effect of playing an object. Effects differ for lands, instants/sorceries, auras and other permanents.
playObjectEffect :: Contextual (Magic ())
playObjectEffect rSelf you = do
    tys <- view (asks (types . objectBase rSelf))
    if landType `isObjectTypesSubsetOf` tys
    then playLandEffect
    else if instantType `isObjectTypesSubsetOf` tys ||
            sorceryType `isObjectTypesSubsetOf` tys
    then playEmptySpellEffect
    else if auraType `isObjectTypesSubsetOf` tys
    then playAuraEffect
    else playPermanentEffect

  where
    playLandEffect :: Magic ()
    playLandEffect = will (PlayLand you rSelf)

    playEmptySpellEffect :: Magic ()
    playEmptySpellEffect = stackSelf (\_ _ -> return ()) rSelf you

    playAuraEffect :: Magic ()
    playAuraEffect = do
      aura <- view (asks (objectBase rSelf))  -- TODO Reevaluate rSelf on the stack?
      ts <- askTarget you $
        checkPermanent (collectEnchantPredicate aura) <?> targetPermanent
      let f :: ObjectRef TyPermanent -> ObjectRef TyStackItem -> PlayerRef -> Magic ()
          f (Battlefield, i) rStackSelf@(Stack, iSelf) _you = do
            self <- view (asks (objectPart . object rStackSelf))
            void $ executeEffect (WillMoveObject (Just (Some Stack, iSelf)) Battlefield (Permanent self Untapped 0 False (Just (Some Battlefield, i)) Nothing))

      stackTargetSelf rSelf you ts f

    collectEnchantPredicate :: Object -> Object -> Bool
    collectEnchantPredicate aura enchanted = gand
      [ hasTypes tys enchanted
      | EnchantPermanent tys <- get staticKeywordAbilities aura ]

    playPermanentEffect :: Magic ()
    playPermanentEffect = stackSelf (\_ _ -> return ()) rSelf you



-- ACTIVATED ABILITIES

loyaltyAbility :: Int -> Contextual (Magic ()) -> ActivatedAbility
loyaltyAbility cost eff = ActivatedAbility
    { abilityActivation = Activation
      { timing    = sorcerySpeed &&* hasAtLeastLoyalty cost
      , available = availableFromBattlefield
      , manaCost  = Just []
      , effect    = \rSelf you -> do
          void $ executeEffects (replicate cost (Will (RemoveCounter rSelf Loyalty)))
          eff rSelf you
      }
    , tapCost     = NoTapCost
    , abilityType = LoyaltyAb
    }
  where
    hasAtLeastLoyalty :: Int -> Contextual (View Bool)
    hasAtLeastLoyalty n rSelf _you = do
      o <- asks (objectBase rSelf)
      return (countCountersOfType Loyalty o >= n)

mkTargetAbility :: PlayerRef -> TargetList a -> (a -> Magic()) -> Magic ()
mkTargetAbility = mkTargetTrigger

mkAbility :: PlayerRef -> Magic() -> Magic ()
mkAbility = mkTrigger

-- | Helper function to create an activated ability (with type 'ActivatedAb') that requires tapping to be activated. Its activation is 'defaultActivation', with its effect set to the specified argument effect.
tapAbility :: Contextual (Magic ()) -> ActivatedAbility
tapAbility = tapAbilityWithCost []

-- | Helper function to create an activated ability (with type 'ActivatedAb') that requires tapping to be activated. Its activation is 'defaultActivation', with its effect set to the specified argument effect and cost.
tapAbilityWithCost :: ManaPool -> Contextual (Magic ()) -> ActivatedAbility
tapAbilityWithCost cost eff = ActivatedAbility
  { abilityType = ActivatedAb
  , tapCost = TapCost
  , abilityActivation = defaultActivation { effect = eff, manaCost = Just cost }
  }

-- | Default activation of an activated ability: available at 'instantSpeed', 'availableFromBattlefield', with a mana cost of 'Just' @[]@ and no effect.
defaultActivation :: Activation
defaultActivation = Activation
  { timing = instantSpeed
  , available = availableFromBattlefield
  , manaCost = Just []
  , effect = \_ _ -> return ()
  }



-- CREATING EFFECTS ON THE STACK


stackSelf :: (ObjectRef TyStackItem -> PlayerRef -> Magic ()) -> Contextual (Magic ())
stackSelf item rSelf you = stackTargetSelf rSelf you (pure ()) (const item)

stackTargetSelf :: Contextual (TargetList a -> (a -> ObjectRef TyStackItem -> PlayerRef -> Magic ()) -> Magic ())
stackTargetSelf rSelf you ts mkItem = do
  -- TODO Make 'you' controller of the object on the stack
  eff <- view (willMoveToStack rSelf (mkItem <$> ts))
  void $ executeEffect eff



-- CONSTRUCTING TRIGGERS


-- | Creates a trigger on the stack under the control of the specified player.
-- The function is applied to the return value of the specified 'TargetList'
-- and put on the stack as a 'StackItem'.
mkTargetTrigger :: PlayerRef -> TargetList a -> (a -> Magic()) -> Magic ()
mkTargetTrigger p ts f = do
  t <- tick
  void $ executeEffect $ WillMoveObject Nothing Stack $
    StackItem (emptyObject t p) ((\x _rStackSelf _you -> f x) <$> ts)


-- | Creates a trigger on the stack under the control of the specified player.
-- The specified program is wrapped in an empty 'TargetList' and passed to
-- 'mkTriggerObject'.
mkTrigger :: PlayerRef -> Magic() -> Magic ()
mkTrigger p f = mkTargetTrigger p (pure ()) (const f)


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
etbWithLoyaltyCounters (WillMoveObject (Just fromRef) Battlefield perm) rSelf _you
  | fromRef == rSelf =
      case get (loyalty . objectPart) perm of
        Just n -> Just $ return [WillMoveObject (Just fromRef) Battlefield (modify (counters . objectPart) (++ replicate n Loyalty) perm)]
        Nothing -> Nothing
etbWithLoyaltyCounters _ _ _ = Nothing

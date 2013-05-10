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
    mkTriggerObject, onSelfETB,
  ) where

import Magic.Core
import Magic.Events
import Magic.IdList (Id)
import Magic.Predicates
import Magic.Target
import Magic.Types
import Magic.Utils (gand, emptyObject)

import Control.Applicative ((<$>), pure)
import Control.Monad (void)

import Data.Boolean ((&&*))
import Data.Label.Pure (get)
import Data.Label.PureM (asks)



-- CAST SPEED


instantSpeed :: Contextual (View Bool)
instantSpeed rSelf rActivator =
  case rSelf of
    (Hand rp, _) -> return (rp == rActivator)
    _            -> return False

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
        self <- asks (object rSelf)
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
        self <- asks (object rSelf)
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
      aura <- view (asks (object rSelf))  -- TODO Reevaluate rSelf on the stack?
      let ok i = collectEnchantPredicate aura <$>
                  asks (object (Battlefield, i))
      ts <- askMagicTargets p (target permanent <?> ok)
      let f :: Id -> ObjectRef -> Magic ()
          f i rStackSelf = do
            self <- view (asks (object rStackSelf))
            let self' = self { _attachedTo = Just (Battlefield, i)
                             , _stackItem = Nothing }
            void $ executeEffect (WillMoveObject (Just rStackSelf) Battlefield self')

      void $ view (willMoveToStack rSelf (f <$> ts)) >>= executeEffect

collectEnchantPredicate :: Object -> Object -> Bool
collectEnchantPredicate aura enchanted = gand
  [ hasTypes tys enchanted
  | EnchantPermanent tys <- get staticKeywordAbilities aura ]

stackTargetlessEffect :: ObjectRef -> (ObjectRef -> Magic ()) -> Magic ()
stackTargetlessEffect rSelf item = do
  eff <- view (willMoveToStack rSelf (pure item))
  void $ executeEffect eff




-- CONSTRUCTING TRIGGERS


-- | Creates a trigger on the stack under the control of the specified player.
mkTriggerObject :: PlayerRef -> StackItem -> Magic ()
mkTriggerObject p item = do
  t <- tick
  void $ executeEffect $ WillMoveObject Nothing Stack $
    (emptyObject t p) { _stackItem = Just item }


-- | Trigger whenever the source object enters the battlefield, executing the
-- argument program.
onSelfETB :: Contextual (Magic ()) -> TriggeredAbilities
onSelfETB mkProgram events rSelf p = return [ mkProgram rSelf p
  | DidMoveObject _ rOther@(Battlefield, _) <- events, rSelf == rOther ]

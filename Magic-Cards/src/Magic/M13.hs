{-# LANGUAGE OverloadedStrings #-}

module Magic.M13 where

import Magic
import Magic.IdList (Id)

import Control.Applicative
import Control.Monad (void)
import Data.Boolean ((&&*))
import Data.Label.Pure (get)
import Data.Label.PureM ((=:), asks)
import Data.Monoid (mconcat)
import qualified Data.Set as Set


instantSpeed :: ObjectRef -> PlayerRef -> View Bool
instantSpeed rSelf rActivator =
  case rSelf of
    (Hand rp, _) -> return (rp == rActivator)
    _            -> return False

sorcerySpeed :: ObjectRef -> PlayerRef -> View Bool
sorcerySpeed rSelf rp = instantSpeed rSelf rp &&* myMainPhase &&* isStackEmpty
  where
    myMainPhase = do
      ap <- asks activePlayer
      as <- asks activeStep
      return (ap == rp && as == MainPhase)

-- | The effect of playing a permanent without targets that uses the stack.
playPermanentEffect :: ObjectRef -> PlayerRef -> Magic ()
playPermanentEffect rSelf _ = void $
    view (willMoveToStack rSelf (pure resolvePermanent)) >>= executeEffect
  where
    resolvePermanent _source = return ()

stackingPlayAbility :: ManaPool -> [AdditionalCost] -> Ability
stackingPlayAbility mc ac =
  Ability
    { available       = \rSelf rActivator -> do
        self <- asks (object rSelf)
        if hasTypes instantType self || Flash `elem` get staticKeywordAbilities self
          then instantSpeed rSelf rActivator
          else sorcerySpeed rSelf rActivator
    , manaCost        = mc
    , additionalCosts = ac
    , effect          = playPermanentEffect
    , isManaAbility   = False
    }

stackTargetlessEffect :: ObjectRef -> (Object -> Magic ()) -> Magic ()
stackTargetlessEffect rSelf item = do
  eff <- view (willMoveToStack rSelf (pure item))
  void $ executeEffect eff

-- | Creates a trigger on the stack under the control of the specified player.
mkTriggerObject :: PlayerRef -> StackItem -> Magic ()
mkTriggerObject p item = void $ executeEffect $ WillMoveObject Nothing Stack $
  (emptyObject undefined p) { _stackItem = Just item }

ajani'sSunstriker :: Card
ajani'sSunstriker = mkCard $ do
  name      =: Just "Ajani's Sunstriker"
  types     =: creatureTypes [Cat, Cleric]
  pt        =: Just (2, 2)
  play      =: Just Ability
    { available       = sorcerySpeed
    , manaCost        = [Just White, Just White]
    , additionalCosts = []
    , effect          = playPermanentEffect
    , isManaAbility   = False
    }
  staticKeywordAbilities =: [Lifelink]

angel'sMercy :: Card
angel'sMercy = mkCard $ do
  name =: Just "Angel's Mercy"
  types =: instantType
  play =: Just Ability
    { available       = instantSpeed
    , manaCost        = [Nothing, Nothing, Just White, Just White]
    , additionalCosts = []
    , effect          = \rSelf rActivator -> stackTargetlessEffect rSelf $ \_ ->
      void $ executeEffect (Will (GainLife rActivator 7))
    , isManaAbility = False
    }

angelicBenediction :: Card
angelicBenediction = mkCard $ do
    name =: Just "Angelic Benediction"
    types =: enchantmentType
    play =: Just Ability
      { available       = sorcerySpeed
      , manaCost        = [Nothing, Nothing, Nothing, Just White]
      , additionalCosts = []
      , effect          = playPermanentEffect
      , isManaAbility   = False
      }
    triggeredAbilities =: [exalted, tapTrigger]
  where
    tapTrigger :: TriggeredAbility
    tapTrigger (Battlefield, _) p events =
      mconcat [
          do
            p' <- asks (object rAttacker .^ controller)
            if p == p'
              then return [mkTapTriggerObject p]
              else return []
        | DidDeclareAttackers _ [rAttacker] <- events ]

    mkTapTriggerObject :: PlayerRef -> Magic ()
    mkTapTriggerObject p = do
        let ok i = hasTypes creatureType <$> asks (object (Battlefield, i))
        ts <- askMagicTargets p (target permanent <?> ok)
        let f :: Id -> Object -> Magic ()
            f i _source = void $ executeEffect $ Will (TapPermanent i)
        mkTriggerObject p (f <$> ts)


exalted :: TriggeredAbility
exalted (Battlefield, _) p events = return [ mkTriggerObject p (boostPT r)
    | DidDeclareAttackers p' [r] <- events, p == p' ]
  where
    boostPT :: ObjectRef -> StackItem
    boostPT r = pure $ \_self ->
      void $ executeEffect $ Will $ InstallContinuousEffect r $
        ContinuousEffect
          { layer       = Layer7c
          , efTimestamp = undefined
          , efEffect    = undefined
          }
exalted _ _ _ = return []

attendedKnight :: Card
attendedKnight = mkCard $ do
    name      =: Just "Attended Knight"
    types     =: creatureTypes [Human, Knight]
    pt        =: Just (2, 2)
    play      =: Just Ability
      { available       = sorcerySpeed
      , manaCost        = [Nothing, Nothing, Just White]
      , additionalCosts = []
      , effect          = playPermanentEffect
      , isManaAbility   = False
      }
    staticKeywordAbilities =: [FirstStrike]
    triggeredAbilities     =: [trigger]
  where
    trigger :: TriggeredAbility
    trigger rSelf p events = return [ mkTriggerObject p (mkSoldier p)
      | DidMoveObject _ rOther@(Battlefield, _) <- events, rSelf == rOther ]

    mkSoldier :: PlayerRef -> StackItem
    mkSoldier p = pure $ \_self -> void $ executeEffect $
      WillMoveObject Nothing Battlefield $ (emptyObject undefined p)
        { _name      = Just "Soldier"
        , _colors    = Set.singleton White
        , _types     = creatureTypes [Soldier]
        , _tapStatus = Just Untapped
        , _pt        = Just (1, 1)
        }

searingSpear :: Card
searingSpear = mkCard $ do
  name  =: Just "Searing Spear"
  types =: instantType
  play  =: Just Ability
    { available = instantSpeed
    , manaCost = [Nothing, Just Red]
    , additionalCosts = []
    , effect = searingSpearEffect
    , isManaAbility = False
    }

searingSpearEffect :: ObjectRef -> PlayerRef -> Magic ()
searingSpearEffect rSelf rActivator = do
  ts <- askMagicTargets rActivator targetCreatureOrPlayer
  let f :: Either Id PlayerRef -> Object -> Magic ()
      f t source = void $ executeEffect $ case t of
        Left i  -> Will (DamageObject source (Battlefield, i) 3 False True)
        Right p -> Will (DamagePlayer source p 3 False True)
  void (view (willMoveToStack rSelf (f <$> ts)) >>= executeEffect)

permanentOrPlayer :: Target -> Maybe (Either Id PlayerRef)
permanentOrPlayer (TargetPlayer p) = Just (Right p)
permanentOrPlayer (TargetObject (Battlefield, i)) = Just (Left i)
permanentOrPlayer _ = Nothing

permanent :: Target -> Maybe Id
permanent (TargetObject (Battlefield, i)) = Just i
permanent _ = Nothing

targetCreatureOrPlayer :: TargetList () (Either Id PlayerRef)
targetCreatureOrPlayer = target permanentOrPlayer <?> ok
  where
    ok t = case t of
      Left i  -> hasTypes creatureType <$> asks (object (Battlefield, i))
      Right _ -> return True
      _       -> return False

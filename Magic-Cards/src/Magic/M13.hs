{-# LANGUAGE OverloadedStrings #-}

module Magic.M13 where

import Magic
import Magic.IdList (Id)
import qualified Magic.IdList as IdList

import Control.Applicative
import Control.Monad (void)
import Data.Boolean ((&&*))
import Data.Label.Pure (get)
import Data.Label.PureM ((=:), asks)
import Data.Maybe (mapMaybe)
import Data.Monoid (mconcat)
import qualified Data.Set as Set



-- HELPER FUNCTIONS: CAST SPEED


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



-- HELPER FUNCTIONS: PLAY ABILITIES


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

stackTargetlessEffect :: ObjectRef -> (Object -> Magic ()) -> Magic ()
stackTargetlessEffect rSelf item = do
  eff <- view (willMoveToStack rSelf (pure item))
  void $ executeEffect eff

-- | Creates a trigger on the stack under the control of the specified player.
mkTriggerObject :: PlayerRef -> StackItem -> Magic ()
mkTriggerObject p item = void $ executeEffect $ WillMoveObject Nothing Stack $
  (emptyObject undefined p) { _stackItem = Just item }



-- HELPER FUNCTIONS: TARGETING


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



-- COMMON ABILITIES


exalted :: TriggeredAbility
exalted (Battlefield, _) p events = return [ mkTriggerObject p (boostPT r)
    | DidDeclareAttackers p' [r] <- events, p == p' ]
  where
    boostPT :: ObjectRef -> StackItem
    boostPT r = pure $ \_self ->
      void $ executeEffect $ Will $ InstallLayeredEffect r $
        TemporaryLayeredEffect
          { temporaryTimestamp = undefined
          , temporaryDuration  = UntilEndOfTurn
          , temporaryEffect    = LayeredEffect
            { affectedObjects  = \_ _ -> return [r]
            , modifications    = [ModifyPT (return (1, 1))]
            }
          }
exalted _ _ _ = return []



-- WHITE CARDS


ajani'sSunstriker :: Card
ajani'sSunstriker = mkCard $ do
  name  =: Just "Ajani's Sunstriker"
  types =: creatureTypes [Cat, Cleric]
  pt    =: Just (2, 2)
  play  =: Just (playPermanent [Just White, Just White])
  staticKeywordAbilities =: [Lifelink]

angel'sMercy :: Card
angel'sMercy = mkCard $ do
  name =: Just "Angel's Mercy"
  types =: instantType
  play =: Just ActivatedAbility
    { available       = instantSpeed
    , manaCost        = [Nothing, Nothing, Just White, Just White]
    , tapCost         = NoTapCost
    , effect          = \rSelf rActivator -> stackTargetlessEffect rSelf $ \_ ->
      void $ executeEffect (Will (GainLife rActivator 7))
    , isManaAbility = False
    }

angelicBenediction :: Card
angelicBenediction = mkCard $ do
    name =: Just "Angelic Benediction"
    types =: enchantmentType
    play =: Just (playPermanent [Nothing, Nothing, Nothing, Just White])
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

attendedKnight :: Card
attendedKnight = mkCard $ do
    name      =: Just "Attended Knight"
    types     =: creatureTypes [Human, Knight]
    pt        =: Just (2, 2)
    play      =: Just (playPermanent [Nothing, Nothing, Nothing, Just White])
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



-- RED CARDS


fervor :: Card
fervor = mkCard $ do
    name              =: Just "Fervor"
    types             =: enchantmentType
    play              =: Just (playPermanent [Nothing, Nothing, Just Red])
    layeredEffects    =: [grantHaste]
  where
    grantHaste = LayeredEffect
      { affectedObjects = \rSelf you ->
          case rSelf of
            (Battlefield, _) ->
              mapMaybe (isAffected you) . IdList.toList <$> asks battlefield
            _ -> return []
      , modifications = [AddStaticKeywordAbility Haste]
      }
    isAffected you (i, o)
      | _controller o == you && hasTypes creatureType o = Just (Battlefield, i)
      | otherwise = Nothing

searingSpear :: Card
searingSpear = mkCard $ do
    name  =: Just "Searing Spear"
    types =: instantType
    play  =: Just ActivatedAbility
      { available     = instantSpeed
      , manaCost      = [Nothing, Just Red]
      , tapCost       = NoTapCost
      , effect        = searingSpearEffect
      , isManaAbility = False
      }
  where
    searingSpearEffect :: Contextual (Magic ())
    searingSpearEffect rSelf rActivator = do
      ts <- askMagicTargets rActivator targetCreatureOrPlayer
      let f :: Either Id PlayerRef -> Object -> Magic ()
          f t source = void $ executeEffect $ case t of
            Left i  -> Will (DamageObject source i 3 False True)
            Right p -> Will (DamagePlayer source p 3 False True)
      void (view (willMoveToStack rSelf (f <$> ts)) >>= executeEffect)

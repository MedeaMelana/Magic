{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Magic.M13 where

import Magic
import qualified Magic.IdList as IdList

import Control.Applicative
import Control.Monad (void)
import Data.Boolean ((&&*))
import Data.Label (get)
import Data.Label.Monadic ((=:), asks)
import Data.Monoid ((<>), mconcat)
import qualified Data.Set as Set
import qualified Data.Text as Text



-- COMMON ABILITIES


exalted :: TriggeredAbilities
exalted events (Some Battlefield, _) p = return [ mkTargetlessTriggerObject p (boostPT r)
    | DidDeclareAttackers p' [Attack r _] <- events, p == p' ]
  where
    boostPT :: ObjectRef TyPermanent -> ObjectRef TyStackItem -> Magic ()
    boostPT (Battlefield, i) _rSelf = do
      t <- tick
      will $ InstallLayeredEffect (Some Battlefield, i) $
        TemporaryLayeredEffect
          { temporaryTimestamp = t
          , temporaryDuration  = UntilEndOfTurn
          , temporaryEffect    = LayeredEffect
            { affectedObjects  = affectSelf
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
  play  =: Just playObject { manaCost = Just [Just White, Just White] }
  staticKeywordAbilities =: [Lifelink]

angel'sMercy :: Card
angel'sMercy = mkCard $ do
  name =: Just "Angel's Mercy"
  types =: instantType
  play =: Just playObject
    { manaCost = Just [Nothing, Nothing, Just White, Just White]
    , effect   = \rSelf you ->
        stackTargetlessEffect rSelf $ \_ ->
          will (GainLife you 7)
    }

angelicBenediction :: Card
angelicBenediction = mkCard $ do
    name =: Just "Angelic Benediction"
    types =: enchantmentType
    play =: Just playObject
      { manaCost = Just [Nothing, Nothing, Nothing, Just White] }
    triggeredAbilities =: exalted <> tapTrigger
  where
    tapTrigger :: TriggeredAbilities
    tapTrigger events (Some Battlefield, _) p =
      mconcat [
          do
            p' <- asks (object rAttacker .^ objectPart .^ controller)
            if p == p'
              then return [mkTapTriggerObject p]
              else return []
        | DidDeclareAttackers _ [Attack rAttacker _] <- events ]
    tapTrigger _ _ _ = return []

    mkTapTriggerObject :: PlayerRef -> Magic ()
    mkTapTriggerObject p = do
        ts <- askTarget p targetCreature
        mkTriggerObject p ts $
          \r _source -> will (TapPermanent r)

attendedKnight :: Card
attendedKnight = mkCard $ do
    name      =: Just "Attended Knight"
    types     =: creatureTypes [Human, Knight]
    pt        =: Just (2, 2)
    play      =: Just playObject
      { manaCost = Just [Nothing, Nothing, Nothing, Just White] }
    staticKeywordAbilities =: [FirstStrike]
    triggeredAbilities     =: trigger
  where
    trigger :: TriggeredAbilities
    trigger = onSelfETB $ \_ p -> mkTargetlessTriggerObject p $ \_self -> do
      t <- tick
      void $ executeEffect $ mkSoldierEffect t p

mkSoldierEffect :: Timestamp -> PlayerRef -> OneShotEffect
mkSoldierEffect t p = WillMoveObject Nothing Battlefield $
    Permanent o Untapped 0 False Nothing Nothing
  where
    o = (emptyObject t p)
        { _name      = Just "Soldier"
        , _colors    = Set.singleton White
        , _types     = creatureTypes [Soldier]
        , _pt        = Just (1, 1)
        }

avenSquire :: Card
avenSquire = mkCard $ do
  name  =: Just "Aven Squire"
  types =: creatureTypes [Bird, Soldier]
  pt    =: Just (1, 1)
  play  =: Just playObject { manaCost = Just [Nothing, Just White] }
  staticKeywordAbilities =: [Flying]
  triggeredAbilities     =: exalted

battleflightEagle :: Card
battleflightEagle = mkCard $ do
    name      =: Just "Battleflight Eagle"
    types     =: creatureTypes [Bird]
    pt        =: Just (2, 2)
    play      =: Just playObject
      { manaCost = Just [Nothing, Nothing, Nothing, Nothing, Just White] }
    staticKeywordAbilities =: [Flying]
    triggeredAbilities     =: onSelfETB createBoostTrigger
  where
    createBoostTrigger :: Contextual (Magic ())
    createBoostTrigger _ p = do
      ts <- askTarget p targetCreature
      mkTriggerObject p ts $ \(Battlefield, i) _source -> do
        t <- tick
        will $
          InstallLayeredEffect (Some Battlefield, i) TemporaryLayeredEffect
            { temporaryTimestamp = t
            , temporaryDuration  = UntilEndOfTurn
            , temporaryEffect    = LayeredEffect
              { affectedObjects  = affectSelf
              , modifications    = [ ModifyPT (return (2, 2))
                                   , AddStaticKeywordAbility Flying
                                   ]
              }
            }

captainOfTheWatch :: Card
captainOfTheWatch = mkCard $ do
    name  =: Just "Captain of the Watch"
    types =: creatureTypes [Human, Soldier]
    pt    =: Just (3, 3)
    play  =: Just playObject { manaCost =
      Just [Nothing, Nothing, Nothing, Nothing, Just White, Just White] }
    staticKeywordAbilities =: [Vigilance]
    layeredEffects         =: [boostSoldiers]
    triggeredAbilities     =: trigger
  where
    boostSoldiers = LayeredEffect
      { affectedObjects = affectRestOfBattlefield $ \you ->
          isControlledBy you &&* hasTypes (creatureTypes [Soldier])
      , modifications = [ AddStaticKeywordAbility Vigilance
                        , ModifyPT (return (1, 1))]
      }

    trigger = onSelfETB $ \_ p -> mkTargetlessTriggerObject p $ \_self -> do
      t <- tick
      void $ executeEffects $ replicate 3 $ mkSoldierEffect t p

captain'sCall :: Card
captain'sCall = mkCard $ do
  name  =: Just "Captain's Call"
  types =: sorceryType
  play  =: Just playObject
    { manaCost = Just [Nothing, Nothing, Nothing, Just White]
    , effect = \rSelf you -> do
        t <- tick
        stackTargetlessEffect rSelf $
          \_ -> void $ executeEffects $ replicate 3 $ mkSoldierEffect t you
    }

divineFavor :: Card
divineFavor = mkCard $ do
    name =: Just "Divine Favor"
    types =: auraType
    staticKeywordAbilities =: [EnchantPermanent creatureType]
    triggeredAbilities =: (onSelfETB $ \_ you -> mkTargetlessTriggerObject you (gainLifeTrigger you))
    layeredEffects =: [boostEnchanted]
    play =: Just playObject { manaCost = Just [Nothing, Just White] }
  where
    gainLifeTrigger you _source = will (GainLife you 3)
    boostEnchanted = LayeredEffect
      { affectedObjects = affectAttached
      , modifications = [ModifyPT (return (1, 3))]
      }



-- RED CARDS


fervor :: Card
fervor = mkCard $ do
    name              =: Just "Fervor"
    types             =: enchantmentType
    play              =: Just playObject
      { manaCost = Just [Nothing, Nothing, Just Red] }
    layeredEffects    =: [grantHaste]
  where
    grantHaste = LayeredEffect
      { affectedObjects = affectBattlefield $ \you ->
          isControlledBy you &&* hasTypes creatureType
      , modifications = [AddStaticKeywordAbility Haste]
      }

searingSpear :: Card
searingSpear = mkCard $ do
    name  =: Just "Searing Spear"
    types =: instantType
    play  =: Just playObject
      { manaCost = Just [Nothing, Just Red]
      , effect   = searingSpearEffect
      }
  where
    searingSpearEffect :: Contextual (Magic ())
    searingSpearEffect rSelf you = do
      ts <- askTarget you targetCreatureOrPlayer
      let f :: Either (ObjectRef TyPermanent) PlayerRef -> ObjectRef TyStackItem -> Magic ()
          f t rStackSelf = do
            self <- view (asks (object rStackSelf .^ objectPart))
            will $ case t of
              Left r  -> DamageObject self r 3 False True
              Right p -> DamagePlayer self p 3 False True
      void (view (willMoveToStack rSelf (f <$> ts)) >>= executeEffect)



-- GREEN CARDS

arborElf :: Card
arborElf = mkCard $ do
    name =: Just "Arbor Elf"
    types =: creatureTypes [Elf, Druid]
    pt =: Just (1, 1)
    play =: Just playObject { manaCost = Just [Just Green] }
    activatedAbilities =: [untapTargetForest]
  where
    untapTargetForest = ActivatedAbility
      { abilityType = ActivatedAb
      , tapCost = TapCost
      , abilityActivation = Activation
        { timing = instantSpeed
        , available = availableFromBattlefield
        , manaCost = Just []
        , effect = \rSelf you -> do
            ts <- askTarget you $ checkPermanent
              (hasTypes (landTypes [Forest])) <?> targetPermanent
            t <- tick
            void $ executeEffect $ WillMoveObject Nothing Stack $
              StackItem (emptyObject t you) (mkEff <$> ts)
        }
      }

    mkEff :: ObjectRef TyPermanent -> ObjectRef TyStackItem -> Magic ()
    mkEff rForest _ = will (UntapPermanent rForest)

bondBeetle :: Card
bondBeetle = mkCard $ do
    name =: Just "Bond Beetle"
    types =: creatureTypes [Insect]
    pt =: Just (0, 1)
    play =: Just playObject { manaCost = Just [Just Green] }
    triggeredAbilities =: onSelfETB createAddCounterTrigger
  where
    createAddCounterTrigger :: Contextual (Magic ())
    createAddCounterTrigger _ p = do
      ts <- askTarget p targetCreature
      mkTriggerObject p ts $ \(Battlefield, i) _source ->
        will $ AddCounter (Some Battlefield, i) Plus1Plus1

garrukPrimalHunter :: Card
garrukPrimalHunter = mkCard $ do
    name =: Just "Garruk, Primal Hunter"
    types =: planeswalkerWithType Garruk
    play =: Just playObject { manaCost =
      Just [Nothing, Nothing, Just Green, Just Green, Just Green] }
    activatedAbilities =: [plusOne, minusThree, minusSix]
    loyalty =: Just 3
    replacementEffects =: [etbWithLoyaltyCounters]
  where
    plusOne = loyaltyAbility 1 $ \_ you -> do
      mkTargetlessTriggerObject you $ \_ -> do
        t <- tick
        let token = simpleCreatureToken t you [Beast] [Green] (3,3)
        void $ executeEffect $ WillMoveObject Nothing Battlefield (Permanent token Untapped 0 False Nothing Nothing)
    minusThree = loyaltyAbility (-3) $ \_ you -> do
      mkTargetlessTriggerObject you $ \_ -> do
        objs <- IdList.elems <$> view (asks battlefield)
        let n = foldr max 0 [ power
                            | o <- objs
                            , get (objectPart .^ controller) o == you
                            , let Just (power, _) = get (objectPart .^ pt) o ]
        void $ executeEffects (replicate n (Will (DrawCard you)))
    minusSix = loyaltyAbility (-6) $ \_ you -> do
      mkTargetlessTriggerObject you $ \_ -> do
        perms <- IdList.elems <$> view (asks battlefield)
        let n = count perms $ \perm ->
                  let o = get objectPart perm
                   in get controller o == you && hasTypes landType o
        t <- tick
        let token = simpleCreatureToken t you [Wurm] [Green] (6,6)
        void $ executeEffects $ replicate n $
          WillMoveObject Nothing Battlefield (Permanent token Untapped 0 False Nothing Nothing)



-- COLORLESS CARDS

chronomaton :: Card
chronomaton = mkCard $ do
    name =: Just "Chronomaton"
    types =: artifactType <> creatureTypes [Golem]
    pt =: Just (1, 1)
    play =: Just playObject { manaCost = Just [Nothing] }
    activatedAbilities =: [addCounter]
  where
    addCounter = ActivatedAbility
      { abilityType = ActivatedAb
      , tapCost = TapCost
      , abilityActivation = Activation
        { timing = instantSpeed
        , available = availableFromBattlefield
        , manaCost = Just []
        , effect = \rSelf you -> do
            t <- tick
            void $ executeEffect $ WillMoveObject Nothing Stack $
              StackItem (emptyObject t you) $ pure $ \rStackSelf ->
                will (AddCounter rSelf Plus1Plus1)
        }
      }



simpleCreatureToken ::
  Timestamp -> PlayerRef -> [CreatureSubtype] -> [Color] -> PT -> Object
simpleCreatureToken t you tys cs pt' =
  (emptyObject t you)
  { _name = Just (Text.intercalate (Text.pack " ") (map textShow tys))
  , _colors = Set.fromList cs
  , _types = creatureTypes tys
  , _pt = Just pt'
  }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Magic.M13 where

import Magic
import Magic.BasicLands (tapToAddMana)
import qualified Magic.IdList as IdList

import Control.Applicative
import Control.Category ((.))
import Control.Monad (void, when)
import Data.Boolean (true, (&&*), (||*))
import Data.Label (get)
import Data.Label.Monadic ((=:), asks)
import Data.Monoid ((<>), mconcat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding ((.))



-- COMMON ABILITIES


exalted :: TriggeredAbilities
exalted events (Some Battlefield, _) p = return [ mkTrigger p (boostPT r)
    | DidDeclareAttackers p' [Attack r _] <- events, p == p' ]
  where
    boostPT :: ObjectRef TyPermanent -> Magic ()
    boostPT (Battlefield, i) = do
      t <- tick
      will $ InstallLayeredEffect (Some Battlefield, i) $
        TemporaryLayeredEffect
          { temporaryTimestamp = t
          , temporaryDuration  = UntilEndOfTurn
          , temporaryEffect    = affectingSelf [ModifyPT (return (1, 1))]
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
    , effect   = stackSelf $ \_ you -> will (GainLife you 7)
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
            p' <- asks (controller . objectPart . object rAttacker)
            if p == p'
              then return [mkTapTriggerObject p]
              else return []
        | DidDeclareAttackers _ [Attack rAttacker _] <- events ]
    tapTrigger _ _ _ = return []

    mkTapTriggerObject :: PlayerRef -> Magic ()
    mkTapTriggerObject p = do
        ts <- askTarget p targetCreature
        mkTargetTrigger p ts (will . TapPermanent)

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
    trigger = onSelfETB $ \_ p -> mkTrigger p $ do
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
      mkTargetTrigger p ts $ \(Battlefield, i) -> do
        t <- tick
        will $
          InstallLayeredEffect (Some Battlefield, i) TemporaryLayeredEffect
            { temporaryTimestamp = t
            , temporaryDuration  = UntilEndOfTurn
            , temporaryEffect    = affectingSelf
                [ModifyPT (return (2, 2)), AddStaticKeywordAbility Flying]
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
    boostSoldiers = LayeredObjectEffect
      { affectedObjects = affectRestOfBattlefield $ \you ->
          isControlledBy you &&* hasTypes (creatureTypes [Soldier])
      , objectModifications = [ AddStaticKeywordAbility Vigilance
                        , ModifyPT (return (1, 1))]
      }

    trigger = onSelfETB $ \_ p -> mkTrigger p $ do
      t <- tick
      void $ executeEffects $ replicate 3 $ mkSoldierEffect t p

captain'sCall :: Card
captain'sCall = mkCard $ do
  name  =: Just "Captain's Call"
  types =: sorceryType
  play  =: Just playObject
    { manaCost = Just [Nothing, Nothing, Nothing, Just White]
    , effect = stackSelf $ \_ you -> do
        t <- tick
        void $ executeEffects $ replicate 3 $ mkSoldierEffect t you
    }

crusaderOfOdric :: Card
crusaderOfOdric = mkCard $ do
    name =: Just "Crusader of Odric"
    types =: creatureTypes [Human, Soldier]
    play =: Just playObject {
      manaCost = Just [Nothing, Nothing, Just White]
    }
    layeredEffects =: [definePT]
  where
    definePT :: LayeredEffect
    definePT = LayeredObjectEffect
      { affectedObjects = affectSelf
      , objectModifications = [DefinePT ( \rSelf ->
        do
          cards <- (map (get objectPart) . IdList.elems) <$> view (asks (battlefield))
          you <- view (asks (controller . objectBase rSelf))
          let c = length $ filter (isControlledBy you &&* hasTypes creatureType) cards
          return (c, c))]
      }

divineFavor :: Card
divineFavor = mkCard $ do
    name =: Just "Divine Favor"
    types =: auraType
    staticKeywordAbilities =: [EnchantPermanent creatureType]
    triggeredAbilities =: gainLifeTrigger
    layeredEffects =: [boostEnchanted]
    play =: Just playObject { manaCost = Just [Nothing, Just White] }
  where
    gainLifeTrigger = onSelfETB $ \_ you ->
      mkTrigger you (will (GainLife you 3))
    boostEnchanted = LayeredObjectEffect
      { affectedObjects = affectAttached
      , objectModifications = [ModifyPT (return (1, 3))]
      }

erase :: Card
erase = mkCard $ do
    name =: Just "Erase"
    types =: instantType
    play =: Just playObject
      { manaCost = Just [Just White]
      , effect = eraseEffect
      }
  where
    eraseEffect :: Contextual (Magic ())
    eraseEffect rSelf you = do
      ts <- askTarget you targetEnchantment
      stackTargetSelf rSelf you ts $ \t _ _ -> do
        ench <- view (asks (objectPart . object t))
        void . executeEffect $ willMoveToExile t ench

guardianLions :: Card
guardianLions = mkCard $ do
    name =: Just "Guardian Lions"
    types =: creatureTypes [Cat]
    pt =: Just (1, 6)
    play =: Just playObject {
      manaCost = Just [Nothing, Nothing, Nothing, Nothing, Just White]
    }
    staticKeywordAbilities =: [Vigilance]

guardiansOfAkrasa :: Card
guardiansOfAkrasa = mkCard $ do
    name =: Just "Guardians of Akrasa"
    types =: creatureTypes [Human, Soldier]
    pt =: Just (0, 4)
    play =: Just playObject {
      manaCost = Just [Nothing, Nothing, Just White]
    }
    staticKeywordAbilities =: [Defender]
    triggeredAbilities =: exalted

healerOfThePride :: Card
healerOfThePride = mkCard $ do
    name =: Just "Healer of the Pride"
    types =: creatureTypes [Cat, Cleric]
    pt =: Just (2, 3)
    play =: Just playObject {
      manaCost = Just [Nothing, Nothing, Nothing, Just White]
    }
    triggeredAbilities =: gainLifeTrigger
  where
    gainLifeTrigger :: TriggeredAbilities
    gainLifeTrigger events (Some Battlefield, _) you =
      mconcat [
          do
            p <- asks (controller . objectPart . object (Battlefield, i))
            isCreatureCard <- hasTypes creatureType <$> asks (objectPart . object (Battlefield, i))
            if you == p && isCreatureCard
              then return [mkTrigger you (will (GainLife you 2))]
              else return []
        | DidMoveObject _ (Some Battlefield, i) <- events ]
    gainLifeTrigger _ _ _ = return []

pacifism :: Card
pacifism = mkCard $ do
    name =: Just "Pacifism"
    types =: auraType
    staticKeywordAbilities =: [EnchantPermanent creatureType]
    play =: Just playObject { manaCost = Just [Nothing, Just White] }
    layeredEffects =: [eff]
  where
    eff = LayeredObjectEffect
      { affectedObjects = affectAttached
      , objectModifications = [ RestrictAllowAttacks selfCantAttack
                        , RestrictAllowBlocks  selfCantBlock ]
      }

pillarfieldOx :: Card
pillarfieldOx = mkCard $ do
    name =: Just "Pillarfield Ox"
    types =: creatureTypes [Ox]
    pt =: Just (2, 4)
    play =: Just playObject {
        manaCost = Just [Nothing, Nothing, Nothing, Just White]
    }

serraAngel :: Card
serraAngel = mkCard $ do
    name =: Just "Serra Angel"
    types =: creatureTypes [Angel]
    pt =: Just (4, 4)
    play =: Just playObject {
      manaCost = Just [Nothing, Nothing, Nothing, Just White, Just White]
    }
    staticKeywordAbilities =: [Flying, Vigilance]

silvercoatLion :: Card
silvercoatLion = mkCard $ do
    name =: Just "Silvercoat Lion"
    types =: creatureTypes [Cat]
    pt =: Just (2, 2)
    play =: Just playObject {
      manaCost = Just [Nothing, Just White]
    }

showOfValor :: Card
showOfValor = mkCard $ do
    name =: Just "Show of Valor"
    types =: instantType
    play =: Just playObject
      { manaCost = Just [Nothing, Just White]
      , effect = showOfValorEffect
      }
  where
    showOfValorEffect rSelf you = do
      ts <- askTarget you targetCreature
      stackTargetSelf rSelf you ts $ \(zr, i) _stackSelf _stackYou -> do
        t <- tick
        will $
          InstallLayeredEffect (Some zr, i) TemporaryLayeredEffect
            { temporaryTimestamp = t
            , temporaryDuration  = UntilEndOfTurn
            , temporaryEffect    = affectingSelf
                [ModifyPT (return (2, 4))]
            }

warFalcon :: Card
warFalcon = mkCard $ do
    name =: Just "War Falcon"
    types =: creatureTypes[Bird]
    pt =: Just (2, 1)
    staticKeywordAbilities =: [Flying]
    play =: Just playObject { manaCost = Just [Just White] }
    allowAttacks =: controlsKnightOrSoldier
  where
    controlsKnightOrSoldier :: [Attack] -> Contextual (View Bool)
    controlsKnightOrSoldier ats (Some Battlefield, i) p = do
        cards <- (map (get objectPart) . IdList.elems) <$> view (asks (battlefield))
        let ok = isControlledBy p &&* (hasTypes (creatureTypes [Knight]) ||* hasTypes (creatureTypes [Soldier]))
        return $ (Battlefield, i) `notElem` map attacker ats || any ok cards
    controlsKnightOrSoldier _ _ _ = true

warPriestOfThune :: Card
warPriestOfThune = mkCard $ do
    name =: Just "War Priest of Thune"
    types =: creatureTypes [Human, Cleric]
    pt =: Just (2, 2)
    play =: Just playObject { manaCost = Just [Nothing, Just White] }
    triggeredAbilities =: onSelfETB warPriestOfThuneTrigger
  where
    warPriestOfThuneTrigger _rSelf you = do
      ench <- askTarget you targetEnchantment
      mkTargetTrigger you ench $ \ref -> do
        shouldDestroy <- askYesNo you "Destroy target enchantment?"
        when shouldDestroy $
          will $ DestroyPermanent ref True


-- BLUE

divination :: Card
divination = mkCard $ do
    name =: Just "Divination"
    types =: sorceryType
    play =: Just playObject
      { manaCost = Just [Nothing, Nothing, Just Blue]
      , effect = stackSelf $ \_ stackYou ->
          void $ executeEffects $ replicate 2 (Will (DrawCard stackYou))
      }

downpour :: Card
downpour = mkCard $ do
    name =: Just "Downpour"
    types =: instantType
    play =: Just playObject
      { manaCost = Just [Nothing, Just Blue]
      , effect = downpourEffect
      }
  where
    downpourEffect rSelf rYou = do
      ts <- askTargetsUpTo 3 rYou targetCreature
      stackTargetSelf rSelf rYou ts $ \t _stackSelf _stackYou ->
        void. executeEffects $ map (Will . TapPermanent) t

faerieInvaders :: Card
faerieInvaders = mkCard $ do
    name =: Just "Faerie Invaders"
    types =: creatureTypes [Faerie, Rogue]
    staticKeywordAbilities =: [Flash]
    play =: Just playObject
      { manaCost = Just $ replicate 4 Nothing ++ [Just Blue] }

mindSculpt :: Card
mindSculpt = mkCard $ do
    name =: Just "Mind Sculpt"
    types =: sorceryType
    play =: Just playObject
      { manaCost = Just [Nothing, Just Blue]
      , effect = mindSculptEffect
      }
  where
    mindSculptEffect :: Contextual (Magic ())
    mindSculptEffect rSelf you = do
      ps <- askTarget you (targetOpponent you)
      stackTargetSelf rSelf you ps $ \p _ _ -> do
        cards <- IdList.toList <$> view (asks (library . player p))
        moveCards (take 7 cards) (Library p) (Graveyard p)

tricksOfTheTrade :: Card
tricksOfTheTrade = mkCard $ do
    name =: Just "Tricks of the Trade"
    types =: auraType
    staticKeywordAbilities =: [EnchantPermanent creatureType]
    layeredEffects =: [boostEnchanted]
    play =: Just playObject
      { manaCost = Just [Nothing, Nothing, Nothing, Just Blue] }
  where
    boostEnchanted = LayeredObjectEffect
      { affectedObjects = affectAttached
      , objectModifications = [ModifyPT (return (2, 0)), RestrictAllowBlocks selfCantBeBlocked]
      }


-- BLACK CARDS

bloodHunterBat :: Card
bloodHunterBat = mkCard $ do
    name =: Just "Bloodhunter Bat"
    types =: creatureTypes [Bat]
    pt =: Just (2, 2)
    play =: Just playObject { manaCost = Just [Nothing, Nothing, Nothing, Just Black] }
    triggeredAbilities =: onSelfETB bloodHunterBatTrigger
  where
    bloodHunterBatTrigger :: Contextual (Magic ())
    bloodHunterBatTrigger _rSelf you = do
      opps <- askTarget you (targetOpponent you)
      mkTargetTrigger you opps $ \opp ->
        void $ executeEffects [Will $ LoseLife opp 2, Will $ GainLife you 2]

cripplingBlight :: Card
cripplingBlight = mkCard $ do
  name =: Just "Crippling Blight"
  types =: auraType
  staticKeywordAbilities =: [EnchantPermanent creatureType]
  layeredEffects =: [boostEnchanted]
  play =: Just playObject
    { manaCost = Just [Just Black] }
  where
    boostEnchanted = LayeredObjectEffect
      { affectedObjects = affectAttached
      , objectModifications = [ModifyPT (return (-1, -1)), RestrictAllowBlocks selfCantBlock]
      }

darkFavor :: Card
darkFavor = mkCard $ do
    name =: Just "Dark Favor"
    types =: auraType
    play =: Just playObject
      { manaCost = Just [Nothing, Just Black] }
    triggeredAbilities =: loseLifeTrigger
    layeredEffects =: [darkFavorEffect]
  where
    loseLifeTrigger = onSelfETB $ \_ you ->
      mkTrigger you (will (LoseLife you 1))
    darkFavorEffect = LayeredObjectEffect
      { affectedObjects = affectAttached
      , objectModifications = [ModifyPT (return (3, 1))]
      }

disentomb :: Card
disentomb = mkCard $ do
  name =: Just "Disentomb"
  types =: sorceryType
  play =: Just playObject
    { manaCost = Just [Just Black]
    , effect = \rSelf you -> do
        ts <- askTarget you (isCreatureCard <?> targetInZone (Graveyard you))
        stackTargetSelf rSelf you ts $ \t@(z, i) _ stackYou -> do
          card <- view (asks (object t))
          void $ executeEffect $
            WillMoveObject (Just (Some z, i)) (Hand stackYou) card
    }
  where
    isCreatureCard :: ObjectRef TyCard -> View Bool
    isCreatureCard r = hasTypes creatureType <$> asks (objectPart . object r)

essenceDrain :: Card
essenceDrain = mkCard $ do
    name =: Just "Essence Drain"
    types =: sorceryType
    play =: Just playObject
      { manaCost = Just $ replicate 4 Nothing ++ [Just Black]
      , effect = essenceDrainEffect
      }
  where
    essenceDrainEffect rSelf you = do
      ts <- askTarget you targetCreatureOrPlayer
      stackTargetSelf rSelf you ts $ \t rStackSelf stackYou -> do
        self <- view (asks (objectPart . object rStackSelf))
        let damageEffect = case t of
              Left r  -> DamageObject self r 3 False True
              Right p -> DamagePlayer self p 3 False True
        void $ executeEffects [Will damageEffect, Will $ GainLife stackYou 3]

ravenousRats :: Card
ravenousRats = mkCard $ do
    name =: Just "Ravenous Rats"
    types =: creatureTypes [Rat]
    pt =: Just (1, 1)
    play =: Just playObject { manaCost = Just [Nothing, Just Black] }
    triggeredAbilities =: onSelfETB ravenousRatsTrigger
  where
    ravenousRatsTrigger _rSelf you = do
      opp <- askTarget you (targetOpponent you)
      mkTargetTrigger you opp $ \o -> do
        oppHand <- IdList.toList <$> view (asks (hand . player o))
        let choices = map (makeChoice o) oppHand
        (someRef, obj) <- askChooseCard o choices
        void $ executeEffect $ WillMoveObject (Just someRef) (Graveyard o) obj
    makeChoice opp (cardId, obj) = let someRef = (Some (Hand opp), cardId)
                               in  (someRef, (someRef, obj))

tormentedSoul :: Card
tormentedSoul = mkCard $ do
    name =: Just "Tormented Soul"
    types =: creatureTypes [Spirit]
    play =: Just playObject { manaCost = Just [Just Black] }
    layeredEffects =: [affectingSelf
      [RestrictAllowBlocks (selfCantBlock &&* selfCantBeBlocked)]]


-- RED CARDS


fervor :: Card
fervor = mkCard $ do
    name              =: Just "Fervor"
    types             =: enchantmentType
    play              =: Just playObject
      { manaCost = Just [Nothing, Nothing, Just Red] }
    layeredEffects    =: [grantHaste]
  where
    grantHaste = LayeredObjectEffect
      { affectedObjects = affectBattlefield $ \you ->
          isControlledBy you &&* hasTypes creatureType
      , objectModifications = [AddStaticKeywordAbility Haste]
      }

moggFlunkies :: Card
moggFlunkies = mkCard $ do
    name =: Just "Mogg Flunkies"
    types =: creatureTypes [Goblin]
    play =: Just playObject { manaCost = Just [Nothing, Just Red] }
    layeredEffects =: [affectingSelf
      [ RestrictAllowAttacks selfCantAttackAlone
      , RestrictAllowBlocks  selfCantBlockAlone  ] ]

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
      stackTargetSelf rSelf you ts $ \t rStackSelf _stackYou -> do
        self <- view (asks (objectPart . object rStackSelf))
        will $ case t of
          Left r  -> DamageObject self r 3 False True
          Right p -> DamagePlayer self p 3 False True


-- GREEN CARDS

arborElf :: Card
arborElf = mkCard $ do
    name =: Just "Arbor Elf"
    types =: creatureTypes [Elf, Druid]
    pt =: Just (1, 1)
    play =: Just playObject { manaCost = Just [Just Green] }
    activatedAbilities =: [untapTargetForest]
  where
    untapTargetForest = tapAbility $ \_ you -> do
      ts <- askTarget you $ checkPermanent
        (hasTypes (landTypes [Forest])) <?> targetPermanent
      mkTargetAbility you ts $ \rForest ->
        will (UntapPermanent rForest)

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
      mkTargetTrigger p ts $ \(Battlefield, i) ->
        will $ AddCounter (Some Battlefield, i) Plus1Plus1

farseek :: Card
farseek = mkCard $ do
    name =: Just "Farseek"
    types =: sorceryType
    play =: Just playObject
      { manaCost = Just [Nothing, Just Green]
      , effect = farseekEffect
      }
  where
    farseekEffect = stackSelf $ \_rStackSelf stackYou -> do
      let validLandTypes = map (landTypes . (:[])) [Plains, Island, Swamp, Mountain]
      maybeId <- searchCard stackYou (Library stackYou) (hasOneOfTypes validLandTypes)
      _ <- case maybeId of
        Just i -> do
          let objectRef = (Some (Library stackYou), i)
          land <- view (asks (objectPart . object (Library stackYou, i)))
          void . executeEffect $
            WillMoveObject (Just objectRef) Battlefield $ Permanent land Tapped 0 False Nothing Nothing
        Nothing -> return ()
      will $ ShuffleLibrary stackYou


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
      mkAbility you $ do
        t <- tick
        let token = simpleCreatureToken t you [Beast] [Green] (3,3)
        void $ executeEffect $ WillMoveObject Nothing Battlefield (Permanent token Untapped 0 False Nothing Nothing)
    minusThree = loyaltyAbility (-3) $ \_ you -> do
      mkAbility you $ do
        objs <- IdList.elems <$> view (asks battlefield)
        let n = foldr max 0 [ power
                            | o <- objs
                            , get (controller . objectPart) o == you
                            , let Just (power, _) = get (pt . objectPart) o ]
        void $ executeEffects (replicate n (Will (DrawCard you)))
    minusSix = loyaltyAbility (-6) $ \_ you -> do
      mkAbility you $ do
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
    addCounter = tapAbility $ \rSelf you ->
      mkTrigger you $ will (AddCounter rSelf Plus1Plus1)

elixirOfImmortality :: Card
elixirOfImmortality = mkCard $ do
    name =: Just "Elixir of Immortality"
    types =: artifactType
    play =: Just playObject { manaCost = Just [Nothing] }
    activatedAbilities =: [crack]
  where
    crack = tapAbilityWithCost [Nothing, Nothing] $
      \rSelf you -> mkTrigger you $ do
        will (GainLife you 5)
        rs <- view $ allRefsInSomeZone (Some (Graveyard you))
        void $ shuffleIntoLibrary (rSelf : rs) [you]

tormod'sCrypt :: Card
tormod'sCrypt = mkCard $ do
    name =: Just "Tormod's Crypt"
    types =: artifactType
    play =: Just playObject { manaCost = Just [] }
    activatedAbilities =: [tapToExile]
  where
    tapToExile = tapAbility $ \(Some Battlefield, i) you -> do
      tp <- askTarget you targetPlayer
      will (Sacrifice (Battlefield, i))
      mkTargetTrigger you tp $ \p -> do
        cards <- IdList.toList <$> view (asks (graveyard . player p))
        moveCards cards (Graveyard p) Exile


moveCards :: [(IdList.Id, ObjectOfType 'TyCard)] -> ZoneRef TyCard -> ZoneRef TyCard -> Magic ()
moveCards cards from to =
  void $ executeEffects
    [ WillMoveObject (Just (Some from, j)) to card
    | (j, card) <- cards
    ]


-- LANDS

dragonskullSummit :: Card
dragonskullSummit = checkLand "Dragonskull Summit" [Black, Red] [Swamp, Mountain]

drownedCatacomb :: Card
drownedCatacomb = checkLand "Drowned Catacomb" [Blue, Black] [Island, Swamp]

glacialFortress :: Card
glacialFortress = checkLand "Glacial Fortress" [White, Blue] [Plains, Island]

rootboundCrag :: Card
rootboundCrag = checkLand "Rootbound Crag" [Red, Green] [Mountain, Forest]

sunpetalGrove :: Card
sunpetalGrove = checkLand "Sunpetal Grove" [White, Green] [Plains, Forest]

checkLand :: Text -> [Color] -> [LandSubtype] -> Card
checkLand n cols tys = mkCard $ do
  name =: Just n
  types =: landType
  play =: Just playObject { manaCost = Just [] }
  activatedAbilities =: map (tapToAddMana . Just) cols
  replacementEffects =: [ etbTappedUnless (map (\ty -> landTypes [ty]) tys) ]

etbTappedUnless :: [ObjectTypes] -> OneShotEffect -> Contextual (Maybe (Magic [OneShotEffect]))
etbTappedUnless tys e@(WillMoveObject (Just r') Battlefield o) r _
  | r' == r = Just $ do
                perms <- (map (get objectPart) . IdList.elems) <$>
                  view (asks battlefield)
                let p = get (owner . objectPart) o
                if null (filter (isControlledBy p &&* gor (map hasTypes tys)) perms)
                then return [WillMoveObject (Just r') Battlefield o { _tapStatus = Tapped } ]
                else return [e]
etbTappedUnless _ _ _ _ = Nothing


simpleCreatureToken ::
  Timestamp -> PlayerRef -> [CreatureSubtype] -> [Color] -> PT -> Object
simpleCreatureToken t you tys cs pt' =
  (emptyObject t you)
  { _name = Just (Text.intercalate (Text.pack " ") (map textShow tys))
  , _colors = Set.fromList cs
  , _types = creatureTypes tys
  , _pt = Just pt'
  }

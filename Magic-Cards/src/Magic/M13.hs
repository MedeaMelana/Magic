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
import Control.Monad (void)
import Data.Boolean ((&&*))
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
    boostEnchanted = LayeredEffect
      { affectedObjects = affectAttached
      , modifications = [ModifyPT (return (1, 3))]
      }



-- BLACK CARDS

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
        void $ executeEffects
          [ WillMoveObject (Just (Some (Graveyard p), j)) Exile card
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

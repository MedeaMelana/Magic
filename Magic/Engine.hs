{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Magic.Engine where

import Magic.Core
import Magic.Events hiding (executeEffect)
import Magic.IdList (Id)
import qualified Magic.IdList as IdList
import Magic.Labels
import Magic.ObjectTypes
import Magic.Predicates
import Magic.Target
import Magic.Types
import Magic.Utils
import Magic.Engine.Types
import Magic.Engine.Events

import Control.Applicative ((<$>))
import Control.Monad (forever, forM_, replicateM_, when, void)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell, execWriterT)
import Data.Label.Pure (get, set)
import Data.Label.PureM (gets, (=:))
import Data.List (nub, intersect, delete)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Traversable (for)
import Prelude hiding (round)



newWorld :: [Deck] -> World
newWorld decks = World
    { _players       = ps
    , _activePlayer  = head playerIds
    , _activeStep    = BeginningPhase UntapStep
    , _time          = 0
    , _turnStructure = ts
    , _exile         = IdList.empty
    , _battlefield   = IdList.empty
    , _stack         = IdList.empty
    , _command       = IdList.empty
    }
  where
    ps = IdList.fromListWithId (\i deck -> newPlayer i deck) decks
    playerIds = IdList.ids ps
    ts = case playerIds of
          -- 103.7a. In a two-player game, the player who plays first skips the draw step of his or her first turn.
          [p1, p2] -> (p1, removeFirst (== BeginningPhase DrawStep) turnSteps)
                        : cycle [(p2, turnSteps), (p1, turnSteps)]
          _        -> cycle (map (, turnSteps) playerIds)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst notOk xs = as ++ bs
  where
    (as, _ : bs) = break notOk xs

turnSteps :: [Step]
turnSteps =
  [ BeginningPhase UntapStep
  , BeginningPhase UpkeepStep
  , BeginningPhase DrawStep
  , MainPhase
  --, CombatPhase BeginningOfCombatStep
  --, CombatPhase DeclareAttackersStep
  --, CombatPhase DeclareBlockersStep
  --, CombatPhase CombatDamageStep
  --, CombatPhase EndOfCombatStep
  --, MainPhase
  , EndPhase EndOfTurnStep
  , EndPhase CleanupStep
  ]

newPlayer :: PlayerRef -> Deck -> Player
newPlayer i deck = Player
  { _life = 20
  , _manaPool = []
  , _prestack = []
  , _library = IdList.fromList (map (\card -> instantiateCard card 0 i) deck)
  , _hand = IdList.empty
  , _graveyard = IdList.empty
  , _maximumHandSize = Just 7
  , _failedCardDraw = False
  }

drawOpeningHands :: [PlayerRef] -> Int -> Engine ()
drawOpeningHands [] _ =
  return ()
drawOpeningHands playerIds 0 =
  forM_ playerIds shuffleLibrary
drawOpeningHands playerIds handSize = do
  mulliganingPlayers <- do
    forM_ playerIds $ \playerId -> do
      moveAllObjects (Hand playerId) (Library playerId)
      shuffleLibrary playerId
      replicateM_ handSize (drawCard playerId)
    for playerIds $ \playerId -> do
      keepHand <- askQuestion playerId AskKeepHand
      if keepHand
        then return Nothing
        else return (Just playerId)
  drawOpeningHands (catMaybes mulliganingPlayers) (handSize - 1)

fullGame :: Engine ()
fullGame = do
  ps <- IdList.ids <$> gets players
  drawOpeningHands ps 7
  forever $ do
    players ~:* set manaPool []
    step <- nextStep
    raise (DidBeginStep step)
    executeStep step
    raise (WillEndStep step)

nextStep :: Engine Step
nextStep = do
  (rp, s : ss) : ts <- gets turnStructure
  turnStructure =: if null ss then ts else (rp, ss) : ts
  activePlayer  =: rp
  activeStep    =: s
  return s



-- Execution of steps

executeStep :: Step -> Engine ()

executeStep (BeginningPhase UntapStep) = do
  -- TODO [502.1]  phasing

  -- [502.2] untap permanents
  rp <- gets activePlayer
  ios <- IdList.filter (isControlledBy rp) <$> gets battlefield
  _ <- for ios $ \(i, _) -> executeEffect (Will (UntapPermanent i))
  return ()

executeStep (BeginningPhase UpkeepStep) = do
  -- TODO [503.1]  handle triggers

  -- [503.2]
  offerPriority

executeStep (BeginningPhase DrawStep) = do
  -- [504.1]
  ap <- gets activePlayer
  executeEffect (Will (DrawCard ap))

  -- TODO [504.2]  handle triggers

  -- [504.3]
  offerPriority

executeStep MainPhase = do
  -- TODO [505.4]  handle triggers

  -- [505.5]
  offerPriority

executeStep (CombatPhase BeginningOfCombatStep) = do
  offerPriority

executeStep (CombatPhase DeclareAttackersStep) = do
  -- TODO [508.1a] declare attackers
  -- TODO [508.1b] declare which player or planeswalker each attacker attacks
  -- TODO [508.1c] check attacking restrictions
  -- TODO [508.1d] check attacking requirements
  -- TODO [508.1e] declare banding
  -- TODO [508.1f] tap attackers
  -- TODO [508.1g] determine costs
  -- TODO [508.1h] allow mana abilities
  -- TODO [508.1i] pay costs
  -- TODO [508.1j] mark creatures as attacking
  -- TODO [508.2]  handle triggers
  offerPriority
  -- TODO [508.6]  potentially skip declare blockers and combat damage steps
  return ()

executeStep (CombatPhase DeclareBlockersStep) = do
  -- TODO [509.1a] declare blockers
  -- TODO [509.1b] check blocking restrictions
  -- TODO [509.1c] check blocking requirements
  -- TODO [509.1d] determine costs
  -- TODO [509.1e] allow mana abilities
  -- TODO [509.1f] pay costs
  -- TODO [509.1g] mark creatures as blocking
  -- TODO [509.1h] mark creatures as blocked
  -- TODO [509.2]  declare attackers' damage assignment order
  -- TODO [509.3]  declare blockers' damage assignment order
  -- TODO [509.4]  handle triggers
  offerPriority
  -- TODO [509.6]  determine new attackers' damage assignment order
  -- TODO [509.7]  determine new blockers' damage assignment order
  return ()

executeStep (CombatPhase CombatDamageStep) = do
  -- TODO [510.1]  assign combat damage
  -- TODO [510.2]  deal damage
  -- TODO [510.3]  handle triggers
  offerPriority
  -- TODO [510.5]  possibly introduce extra combat damage step for first/double strike
  return ()

executeStep (CombatPhase EndOfCombatStep) = do
  -- TODO [511.1]  handle triggers

  -- [511.2]
  offerPriority

  -- TODO [511.3]  remove creatures from combat
  return ()

executeStep (EndPhase EndOfTurnStep) = do
  -- TODO [513.1]  handle triggers
  
  -- [513.2]
  offerPriority

executeStep (EndPhase CleanupStep) = do
  -- TODO [514.1]  discard excess cards
  -- TODO [514.2]  remove damage from permanents
  -- TODO [514.3]  handle triggers; check state-based actions; possibly offer priority
  return ()






offerPriority :: Engine ()
offerPriority = gets activePlayer >>= fullRoundStartingWith
  where
    fullRoundStartingWith p = do
      -- TODO do this in a loop
      checkSBAs
      processPrestacks
      mAction <- playersStartingWith p >>= partialRound
      case mAction of
        Just (p, action) -> do
          executePriorityAction p action
          fullRoundStartingWith p
        Nothing -> do
          st <- gets stack
          case IdList.head st of
            Nothing -> return ()
            Just (i, _) -> do
              resolve i
              offerPriority

    partialRound ((p, _):ps) = do
      actions <- collectPriorityActions p
      mAction <- askQuestion p (AskPriorityAction actions)
      case mAction of
        Just action -> return (Just (p, action))
        Nothing -> partialRound ps
    partialRound [] = return Nothing

checkSBAs :: Engine ()
checkSBAs = do
  sbas <- collectSBAs
  sbas' <- concat <$> for sbas applyReplacementEffects
  forM_ sbas' executeEffect

collectSBAs :: Engine [OneShotEffect]
collectSBAs = execWriterT $ do
    checkPlayers
    checkBattlefield
    -- TODO [704.5d]
    -- TODO [704.5e]
    -- TODO [704.5u]
    -- TODO [704.5v]
    -- TODO [704.5w]
  where
    checkPlayers = do
      -- [704.5a]
      -- [704.5b]
      -- TODO [704.5c]
      -- TODO [704.5t]
      ips <- IdList.toList <$> lift (gets players)
      forM_ ips $ \(i,p) -> do
        when (get life p <= 0 || get failedCardDraw p) $
          tell [Will (LoseGame i)]

    checkBattlefield = do
      ios <- IdList.toList <$> lift (gets battlefield)
      forM_ ios $ \(i,o) -> do

        -- Check creatures
        when (hasTypes creatureType o) $ do

          -- [704.5f]
          let hasNonPositiveToughness = maybe False (<= 0) (get toughness o)
          when hasNonPositiveToughness $ tell [willMoveToGraveyard i o]

          -- [704.5g]
          -- [704.5h]
          let hasLethalDamage =
                case (get toughness o, get damage o) of
                  (Just t, Just d) -> t > 0 && d >= t
                  _                -> False
          when (hasLethalDamage || get deathtouched o) $
            tell [Will (DestroyPermanent i True)]

        -- [704.5i]
        when (hasTypes planeswalkerType o && countCountersOfType Loyalty o == 0) $
          tell [willMoveToGraveyard i o]

      -- TODO [704.5j]
      -- TODO [704.5k]
      -- TODO [704.5m]
      -- TODO [704.5n]
      -- TODO [704.5p]
      -- TODO [704.5q]
      -- TODO [704.5r]
      -- TODO [704.5s]

-- | Ask players to put pending items on the stack in APNAP order. [405.3]
processPrestacks :: Engine ()
processPrestacks = do
  ips <- apnap
  forM_ ips $ \(i,p) -> do
    let pending = get prestack p
    when (not (null pending)) $ do
      pending' <- askQuestion i (AskReorder pending)
      forM_ pending' $ \mkStackObject -> do
        stackObject <- executeMagic mkStackObject
        stack ~: IdList.cons stackObject

resolve :: Id -> Engine ()
resolve i = do
  o <- gets (stack .^ listEl i)
  let Just item = get stackItem o
  let (_, mkEffects) = evaluateTargetList item
  executeMagic (mkEffects o) >>= mapM_ executeEffect
  -- if the object is now still on the stack, move it to the appropriate zone
  let o' = set stackItem Nothing o
  if (hasTypes instantType o || hasTypes sorceryType o)
    then void (moveObject (Stack, i) (Graveyard (get controller o)) o')
    else void (moveObject (Stack, i) Battlefield o')

collectPriorityActions :: PlayerRef -> Engine [PriorityAction]
collectPriorityActions p = do
  as <- map ActivateAbility <$> collectAvailableActivatedAbilities (const True) p
  plays <- map PlayCard <$> collectPlayableCards p
  return (as <> plays)

collectAvailableActivatedAbilities :: (ClosedAbility -> Bool) -> PlayerRef -> Engine [ActivatedAbilityRef]
collectAvailableActivatedAbilities predicate p = do
  objects <- view allObjects
  execWriterT $ do
    for objects $ \(r,o) -> do
      for (zip [0..] (get activatedAbilities o)) $ \(i, ability) -> do
        ok <- lift (shouldOfferAbility ability r p)
        when (predicate (ability r p) && ok) (tell [(r, i)])

collectPlayableCards :: PlayerRef -> Engine [ObjectRef]
collectPlayableCards p = do
  objects <- view allObjects
  execWriterT $ do
    for objects $ \(r,o) -> do
      let Just playAbility = get play o
      ok <- lift (shouldOfferAbility playAbility r p)
      when ok (tell [r])

shouldOfferAbility :: Ability -> ObjectRef -> PlayerRef -> Engine Bool
shouldOfferAbility ability rSource rActivator = do
  let closedAbility = ability rSource rActivator
  abilityOk <- executeMagic (view (get available closedAbility))
  payCostsOk <- canPayAdditionalCosts rSource rActivator (get additionalCosts closedAbility)
  return (abilityOk && payCostsOk)

activateAbility :: Ability -> ObjectRef -> PlayerRef -> Engine ()
activateAbility ability rSource rActivator  = do
  let closedAbility = ability rSource rActivator
  offerManaAbilitiesToPay rActivator (get manaCost closedAbility)
  forM_ (get additionalCosts closedAbility) (payAdditionalCost rSource rActivator)
  executeMagic (get effect closedAbility) >>= mapM_ executeEffect

executePriorityAction :: PlayerRef -> PriorityAction -> Engine ()
executePriorityAction p a = do
  case a of
    PlayCard r -> do
      Just ability <- gets (object r .^ play)
      activateAbility ability r p
    ActivateAbility (r, i) -> do
      abilities <- gets (object r .^ activatedAbilities)
      activateAbility (abilities !! i) r p

offerManaAbilitiesToPay :: PlayerRef -> ManaPool -> Engine ()
offerManaAbilitiesToPay _ []   = return ()
offerManaAbilitiesToPay p cost = do
  amas <- map ActivateManaAbility <$>
          collectAvailableActivatedAbilities (get isManaAbility) p
  pool <- gets (player p .^ manaPool)
  let pms =
        if Nothing `elem` cost
          then map PayManaFromManaPool (nub pool)  -- there is at least 1 colorless mana to pay
          else map PayManaFromManaPool (nub pool `intersect` cost)
  action <- askQuestion p (AskManaAbility cost (amas <> pms))
  case action of
    PayManaFromManaPool mc -> do
      executeEffect (Will (SpendFromManaPool p [mc]))
      if mc `elem` cost
        then offerManaAbilitiesToPay p (delete mc cost)
        else offerManaAbilitiesToPay p (delete Nothing cost)
    ActivateManaAbility (r, i) -> do
      abilities <- gets (object r .^ activatedAbilities)
      activateAbility (abilities !! i) r p
      offerManaAbilitiesToPay p cost

canPayAdditionalCosts :: ObjectRef -> PlayerRef -> [AdditionalCost] -> Engine Bool
canPayAdditionalCosts rSource _ [] = return True
canPayAdditionalCosts rSource _ (c:cs) =
  case (rSource, c) of
    ((Battlefield, i), TapSelf) -> do
      ts <- gets (object (Battlefield, i) .^ tapStatus)
      return (ts == Just Untapped)
    (_, TapSelf) -> return False

payAdditionalCost :: ObjectRef -> PlayerRef -> AdditionalCost -> Engine ()
payAdditionalCost rSource p c =
  case c of
    TapSelf -> case rSource of (Battlefield, i) -> void (executeEffect (Will (TapPermanent i)))

-- | Returns player IDs in APNAP order (active player, non-active player).
apnap :: Engine [(PlayerRef, Player)]
apnap = gets activePlayer >>= playersStartingWith

playersStartingWith :: PlayerRef -> Engine [(PlayerRef, Player)]
playersStartingWith p = do
  (ps, qs) <- break ((== p) . fst) . IdList.toList <$> gets players
  return (qs ++ ps)

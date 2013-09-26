{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Magic.Target (
    -- * Types
    TargetList(..), EntityRef(..),

    -- * Producing target lists
    target, (<?>),

    -- * Compiling target lists
    evaluateTargetList, askMagicTargets,
    permanent, permanentOrPlayer, targetCreatureOrPlayer, targetPlayer
  ) where

import qualified Magic.IdList as IdList
import Magic.IdList (Id)
import Magic.Core
import Magic.Types
import Magic.Predicates
import Magic.ObjectTypes
import Magic.Some
import Magic.Labels ((.^))

import Control.Applicative
import Control.Monad (forM, filterM)
import Data.Label.PureM (asks)


evaluateTargetList :: TargetList EntityRef a -> ([EntityRef], Maybe a)
evaluateTargetList (Nil x) = ([], Just x)
evaluateTargetList (Snoc xs cast t) = (ts ++ [t], mf <*> cast t)
  where (ts, mf) = evaluateTargetList xs
evaluateTargetList (Test f _ xs) = (ts, f <$> mx)
  where (ts, mx) = evaluateTargetList xs

target :: (EntityRef -> Maybe a) -> TargetList () a
target f = Snoc (Nil id) f ()

infixl 4 <?>
(<?>) :: TargetList t a -> (a -> View Bool) -> TargetList t a
xs <?> ok = Test id ok xs

askTargets :: forall a. ([EntityRef] -> Magic EntityRef) -> [EntityRef] -> TargetList () a -> Magic (TargetList EntityRef a, a)
askTargets choose = askTargets' (const (return True))
  where
    askTargets' :: forall b. (b -> View Bool) -> [EntityRef] -> TargetList () b -> Magic (TargetList EntityRef b, b)
    askTargets' ok ts scheme =
      case scheme of
        Nil x -> return (Nil x, x)
        Snoc tsf cast () -> do
          (tsf', f) <- askTargets' (const (return True)) ts tsf
          eligibleTargets <- view $ flip filterM ts $ \t -> do
            case cast t of
              Just y -> ok (f y)
              Nothing -> return False
          chosen <- choose eligibleTargets
          let Just x = cast chosen
          return (Snoc tsf' cast chosen, f x)
        Test f ok' tsx -> do
          (tsx', x) <- askTargets' (\x -> (&&) <$> ok (f x) <*> ok' x) ts tsx
          return (f <$> tsx', f x)

askMagicTargets :: PlayerRef -> TargetList () a -> Magic (TargetList EntityRef a)
askMagicTargets p ts = do
  ats <- allTargets
  fst <$> askTargets (askQuestion p . AskTarget) ats ts

allTargets :: Magic [EntityRef]
allTargets = do
  ps <- IdList.ids <$> view (asks players)
  let szrs = [Some Exile, Some Battlefield, Some Stack, Some Command] ++
            concat [ [Some (Library p), Some (Hand p), Some (Graveyard p)] | p <- ps ]
  oss <- forM szrs $ \(Some zr) -> do
    os <- IdList.ids <$> view (asks (compileZoneRef zr))
    return (map (\o -> (Some zr, o)) os)
  return (map PlayerRef ps ++ map ObjectRef (concat oss))



-- HELPER FUNCTIONS: TARGETING


permanentOrPlayer :: EntityRef -> Maybe (Either Id PlayerRef)
permanentOrPlayer (PlayerRef p) = Just (Right p)
permanentOrPlayer (ObjectRef (Some Battlefield, i)) = Just (Left i)
permanentOrPlayer _ = Nothing

permanent :: EntityRef -> Maybe Id
permanent (ObjectRef (Some Battlefield, i)) = Just i
permanent _ = Nothing

targetCreatureOrPlayer :: TargetList () (Either Id PlayerRef)
targetCreatureOrPlayer = target permanentOrPlayer <?> ok
  where
    ok t = case t of
      Left i  -> hasTypes creatureType <$> asks (object (Battlefield, i) .^ objectPart)
      Right _ -> return True

targetPlayer :: TargetList () PlayerRef
targetPlayer = target cast
  where
    cast (PlayerRef p) = Just p
    cast _ = Nothing

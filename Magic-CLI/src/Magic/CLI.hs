{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Magic.CLI where

import Magic
import Magic.Engine.Types (GameOver(..))
import Magic.Description (Description(..), describeWorld, describeZone, describePriorityAction,
  describeEvent, describeEntityRef, describeManaPool, describePayManaAction, describeObjectName, describeObjectNameByRef)

import Control.Monad (forM_)
import Control.Monad.Operational (ProgramT, ProgramViewT(..), viewT)
import Control.Monad.Random (RandT, StdGen, evalRandT, newStdGen)
import Control.Monad.Reader (runReader)
import Control.Monad.State (evalStateT)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Safe
import qualified System.IO as IO
import qualified Data.Set as S

runGame :: [Deck] -> IO ()
runGame decks = do
  program <- evalRandTIO (evalStateT (runEngine fullGame) (newWorld decks))
  askQuestions S.empty program

evalRandTIO :: Monad m => RandT StdGen m a -> IO (m a)
evalRandTIO p = evalRandT p `fmap` newStdGen

desc :: World -> Description -> Text
desc w d = runReader (runViewT (runDescription d)) w

data PassType = SinglePass | EndOfTurnPass


askQuestions :: S.Set PlayerRef -> ProgramT Interact (Either GameOver) () -> IO ()
askQuestions playerPasses = eval . viewT
  where
    skipOr p world k action =
        case (p `S.member` playerPasses, _activeStep world) of
          (True, EndPhase EndOfTurnStep) -> askQuestions (S.delete p playerPasses) k
          (True, _) -> askQuestions playerPasses k
          (False, _) -> action
    eval (Left gameOver) = case gameOver of
      GameWin p -> Text.putStrLn ("Player " <> showText p <> " wins!")
      GameDraw  -> Text.putStrLn "The game is a draw"
      ErrorWithMessage message -> Text.putStrLn ("Engine failed with error: " <> message)
      UnknownError -> Text.putStrLn "Unknown error"
    eval (Right program) = case program of
      Return x -> return x
      Debug t :>>= k -> do
        Text.putStrLn ("[DEBUG] " <> t)
        askQuestions playerPasses (k ())
      LogEvents source es world :>>= k -> do
        IO.putStrLn ("[EVENT] Caused by " <> show source)
        forM_ es $ \e -> Text.putStrLn (desc world (">>> " <> describeEvent e))
        askQuestions playerPasses (k ())
      AskQuestion p world AskKeepHand :>>= k -> do
        Text.putStrLn (desc world (describeZone (Some (Hand p))))
        chosen <- offerOptions p "Would you like to keep your hand?" [("Keep hand", True), ("Take mulligan", False)]
        askQuestions playerPasses (k chosen)
      AskQuestion p world (AskPriorityAction actions) :>>= k -> do
        skipOr p world (k Nothing) $ do
            Text.putStrLn (desc world describeWorld)
            let singlePass = ("Pass", Left SinglePass)
            let multiPass = ("Skip to next turn", Left EndOfTurnPass)
            let nonPass = [ (desc world (describePriorityAction action), Right action) | action <- actions ]
            chosen <- offerOptions p "What would you like to do?" (singlePass : multiPass : nonPass)
            case chosen of
                Left SinglePass -> askQuestions playerPasses (k Nothing)
                Left EndOfTurnPass -> askQuestions (S.insert p playerPasses) (k Nothing)
                Right ch -> askQuestions playerPasses (k $ Just ch)
      AskQuestion p world (AskTarget ts) :>>= k -> do
        t <- offerOptions p "Choose target:" [ (desc world (describeEntityRef t), t) | t <- ts ]
        askQuestions playerPasses (k t)
      AskQuestion p world (AskManaAbility cost actions) :>>= k -> do
        let costDesc = desc world (describeManaPool cost)
        let options = [ (desc world (describePayManaAction action), action) | action <- actions ]
        chosen <- offerOptions p ("Pay " <> costDesc) options
        askQuestions playerPasses (k chosen)
      AskQuestion p world (AskPickTrigger lkis) :>>= k -> do
        let options = [ (desc world (describeObjectName o), i) | (i, (_, o)) <- zip [0..] lkis ]
        chosen <- offerOptions p "Choose trigger to put on the stack:" options
        askQuestions playerPasses (k chosen)
      AskQuestion p world (AskAttackers ats defs) :>>= k -> do
        skipOr p world (k []) $ do
          Text.putStrLn "Declare attackers:"
          chosen <- declareAttackers p world ats defs
          askQuestions playerPasses (k chosen)

declareAttackers :: PlayerRef -> World -> [ObjectRef TyPermanent] -> [EntityRef] -> IO [Attack]
declareAttackers p world [] defs = return []
declareAttackers p world ((Battlefield, i):ats) defs = do
  let q = "Who should " <> desc world (describeObjectNameByRef (Some Battlefield, i)) <> " attack?"
  let options = [ ("Attack " <> desc world (describeEntityRef def), Just def) | def <- defs ]
  let noone = [("Don't attack", Nothing)]
  mchosen <- offerOptions p q (options ++ noone)
  rest <- declareAttackers p world ats defs
  return $ case mchosen of
    Just chosen -> (Attack (Battlefield, i) chosen) : rest
    Nothing     -> rest

showText :: Show a => a -> Text
showText = Text.pack . show

offerOptions :: PlayerRef -> Text -> [(Text, a)] -> IO a
offerOptions p q opts = do
    Text.putStrLn ("[Player " <> showText p <> "] " <> q)
    forM_ (zip [(1 :: Int) ..] opts) $ \(i, (opt, _)) ->
      Text.putStrLn (showText i <> ") " <> opt)
    getAnswer
  where
    getAnswer = do
      Text.putStr "> "
      input <- getLine
      case (pred `fmap` readMay input) >>= atMay opts of
        Nothing -> do
          Text.putStrLn "Invalid option"
          getAnswer
        Just opt ->
          return (snd opt)

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Magic.CLI where

import Magic.Engine (fullGame, newWorld)
import Magic.Engine.Types (runEngine, GameOver(..))
import Magic.Types hiding (view)
import Magic.Description (Description(..), describeWorld, describeZone, describePriorityAction,
  describeEvent, describeTarget, describeManaPool, describePayManaAction, describeObjectName)

import Control.Monad (forM_)
import Control.Monad.Operational (ProgramT, ProgramViewT(..), viewT)
import Control.Monad.Random
import Control.Monad.Reader (runReader)
import Control.Monad.State (evalStateT)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Safe
import qualified System.IO as IO

runGame :: [Deck] -> IO ()
runGame decks = do
  program <- evalRandTIO (evalStateT (runEngine fullGame) (newWorld decks))
  askQuestions program

evalRandTIO :: Monad m => RandT StdGen m a -> IO (m a)
evalRandTIO p = evalRandT p `fmap` newStdGen

desc :: World -> Description -> Text
desc w d = runReader (runViewT (runDescription d)) w

askQuestions :: ProgramT Interact (Either GameOver) () -> IO ()
askQuestions = eval . viewT
  where
    eval (Left gameOver) = case gameOver of
      GameWin p -> Text.putStrLn ("Player " <> showText p <> " wins!")
      GameDraw  -> Text.putStrLn "The game is a draw"
      ErrorWithMessage message -> Text.putStrLn ("Engine failed with error: " <> message)
      UnknownError -> Text.putStrLn "Unknown error"
    eval (Right program) = case program of
      Return x -> return x
      Debug t :>>= k -> do
        Text.putStrLn ("[DEBUG] " <> t)
        askQuestions (k ())
      LogEvents source es world :>>= k -> do
        IO.putStrLn ("[EVENT] Caused by " <> show source)
        forM_ es $ \e -> Text.putStrLn (desc world (">>> " <> describeEvent e))
        askQuestions (k ())
      AskQuestion p world AskKeepHand :>>= k -> do
        Text.putStrLn (desc world (describeZone (Hand p)))
        chosen <- offerOptions p "Would you like to keep your hand?" [("Keep hand", True), ("Take mulligan", False)]
        askQuestions (k chosen)
      AskQuestion p world (AskPriorityAction actions) :>>= k -> do
        Text.putStrLn (desc world describeWorld)
        let pass = ("Pass", Nothing)
        let nonPass = [ (desc world (describePriorityAction action), Just action) | action <- actions ]
        chosen <- offerOptions p "What would you like to do?" (pass : nonPass)
        askQuestions (k chosen)
      AskQuestion p world (AskTarget ts) :>>= k -> do
        t <- offerOptions p "Choose target:" [ (desc world (describeTarget t), t) | t <- ts ]
        askQuestions (k t)
      AskQuestion p world (AskManaAbility cost actions) :>>= k -> do
        let costDesc = desc world (describeManaPool cost)
        let options = [ (desc world (describePayManaAction action), action) | action <- actions ]
        chosen <- offerOptions p ("Pay " <> costDesc) options
        askQuestions (k chosen)
      AskQuestion p world (AskPickTrigger lkis) :>>= k -> do
        let options = [ (desc world (describeObjectName o), i) | (i, (_, o)) <- zip [0..] lkis ]
        chosen <- offerOptions p "Choose trigger to put on the stack:" options
        askQuestions (k chosen)


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

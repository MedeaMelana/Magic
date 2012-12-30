{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Magic.CLI where

import Magic.Engine (fullGame, newWorld)
import Magic.Types hiding (view)
import Magic.Description (Description(..), describeWorld, describeHand, describePriorityAction, describeEvent)

import Control.Monad (forM_)
import Control.Monad.Operational (Program, ProgramViewT(..), view)
import Control.Monad.Random
import Control.Monad.Reader (runReader)
import Control.Monad.State (evalStateT)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Safe

runGame :: [Deck] -> IO ()
runGame decks = do
  program <- evalRandTIO (evalStateT fullGame (newWorld decks))
  askQuestions program

evalRandTIO :: Monad m => RandT StdGen m a -> IO (m a)
evalRandTIO p = evalRandT p `fmap` newStdGen

desc :: World -> Description -> Text
desc w d = runReader (runDescription d) w

askQuestions :: Program Interact a -> IO a
askQuestions = eval . view
  where
    eval xs = case xs of
      Return x -> return x
      Debug t :>>= k -> do
        Text.putStrLn t
        askQuestions (k ())
      LogEvent e world :>>= k -> do
        Text.putStrLn (desc world (">>> " <> describeEvent e))
        askQuestions (k ())
      AskQuestion p world AskKeepHand :>>= k -> do
        Text.putStrLn (desc world ("Your hand: \n" <> describeHand p <> "\n"))
        chosen <- offerOptions p "Would you like to keep your hand?" [("Keep hand", True), ("Take mulligan", False)]
        askQuestions (k chosen)
      AskQuestion p world (AskPriorityAction actions) :>>= k -> do
        Text.putStrLn (desc world describeWorld <> "\n")
        let pass = ("Pass", Nothing)
        let nonPass = [ (desc world (describePriorityAction action), Just action) | action <- actions ]
        chosen <- offerOptions p "What would you like to do?" (pass : nonPass)
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

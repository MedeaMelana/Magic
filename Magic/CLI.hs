{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Magic.CLI where

import Magic.Core
import Magic.Engine (fullGame, newWorld)
import Magic.IdList (Id, toList)
import Magic.Types hiding (view)
import Magic.Labels

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Operational (Program, ProgramViewT(..), view)
import Control.Monad.Random
import Control.Monad.State (evalStateT)
import Data.Label.Pure (get)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Text (unpack)
import Safe

runGame :: [Deck] -> IO ()
runGame decks = do
  program <- evalRandTIO (evalStateT fullGame (newWorld decks))
  askQuestions program

evalRandTIO :: Monad m => RandT StdGen m a -> IO (m a)
evalRandTIO p = evalRandT p `fmap` newStdGen

askQuestions :: Program AskedQuestion a -> IO a
askQuestions = eval . view
  where
    eval xs = case xs of
      Return x -> return x
      AskedQuestion p world AskKeepHand :>>= k -> do
        putStrLn ("Your hand: " ++ describeHand p world)
        chosen <- offerOptions p "Would you like to keep your hand?" [("Keep hand", True), ("Take mulligan", False)]
        askQuestions (k chosen)
      AskedQuestion p world (AskPriorityAction actions) :>>= k -> do
        putStr (describeWorld world)
        let pass = ("Pass", Nothing)
        let nonPass = [ (describePriorityAction action, Just action) | action <- actions ]
        chosen <- offerOptions p "What would you like to do?" (pass : nonPass)
        askQuestions (k chosen)

offerOptions :: PlayerRef -> String -> [(String, a)] -> IO a
offerOptions p q opts = do
    putStrLn ("[Player " ++ show p ++ "] " ++ q)
    forM_ (zip [1..] opts) $ \(i, (opt, _)) ->
      putStrLn (show i ++ ") " ++ opt)
    getAnswer
  where
    getAnswer = do
      putStr "> "
      input <- getLine
      case (pred `fmap` readMay input) >>= atMay opts of
        Nothing -> do
          putStrLn "Invalid option"
          getAnswer
        Just opt ->
          return (snd opt)

describePriorityAction :: PriorityAction -> String
describePriorityAction a =
  case a of
    PlayCard ro -> "Play " ++ describeObjectRef ro
    ActivateAbility ro i -> "Activate ability " ++ show i ++ " of " ++ show ro

describeObjectRef :: ObjectRef -> String
describeObjectRef = show

describeWorld :: World -> String
describeWorld world = unlines
  [ "Player " ++ show (get activePlayer world) ++ "'s turn"
  , show (get activeStep world)
  ]

describeHand :: PlayerRef -> World -> String
describeHand p world = intercalate ", " $
  map describeObject (toList (get (player p .^ hand) world))

describeObject :: (Id, Object) -> String
describeObject (i, o) =
  case get name o of
    Just n -> "#" ++ show i ++ " " ++ unpack n
    Nothing -> "#" ++ show i

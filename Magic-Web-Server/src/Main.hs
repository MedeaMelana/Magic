{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import Magic
import Magic.Description (Description(..), describeWorld, describeZone, describePriorityAction,
  describeTarget, describeManaPool, describePayManaAction, describeObjectName)
import Magic.Engine.Types
import Magic.BasicLands
import Magic.M13
import Magic.Json ()

import Control.Monad (forM_)
import Control.Monad.Operational (ProgramT, ProgramViewT(..), viewT)
import Control.Monad.Random (evalRandT, newStdGen, RandT, StdGen)
import Control.Monad.Reader (runReader)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (liftIO)

import Data.Aeson (encode)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import Network.WebSockets (WebSockets, Hybi00, Request, TextProtocol,
  acceptRequest, sendTextData, receiveData, runServer)

import Safe (readMay, atMay)


main :: IO ()
main = runServer "0.0.0.0" 8080 app

app :: Request -> WebSockets Hybi00 ()
app req = do
  acceptRequest req
  runFullGame [whiteDeck, redDeck]

redDeck :: Deck
redDeck = replicate 18 mountain <> replicate 42 searingSpear

whiteDeck :: Deck
whiteDeck = replicate 18 plains <> replicate 42 attendedKnight

runFullGame :: [Deck] -> WebSockets Hybi00 ()
runFullGame decks = do
  program <- liftIO $ evalRandTIO (evalStateT (runEngine fullGame) (newWorld decks))
  askQuestions program

evalRandTIO :: Monad m => RandT StdGen m a -> IO (m a)
evalRandTIO p = evalRandT p `fmap` newStdGen

askQuestions :: ProgramT Interact (Either GameOver) () -> WebSockets Hybi00 ()
askQuestions = eval . viewT
  where
    eval (Left gameOver) = case gameOver of
      GameWin p -> sendText ("Player " <> showText p <> " wins!")
      GameDraw  -> sendText "The game is a draw"
      ErrorWithMessage message -> sendText ("Engine failed with error: " <> message)
      UnknownError -> sendText "Unknown error"
    eval (Right program) = case program of
      Return x -> return x
      Debug t :>>= k -> do
        sendText ("[DEBUG] " <> t)
        askQuestions (k ())
      instr@(LogEvents _ _ _) :>>= k -> do
        sendTextData (encode instr)
        askQuestions (k ())
      AskQuestion p world AskKeepHand :>>= k -> do
        sendText (desc world (describeZone (Hand p)))
        chosen <- offerOptions p "Would you like to keep your hand?" [("Keep hand", True), ("Take mulligan", False)]
        askQuestions (k chosen)
      AskQuestion p world (AskPriorityAction actions) :>>= k -> do
        sendText (desc world describeWorld)
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

desc :: World -> Description -> Text
desc w d = runReader (runViewT (runDescription d)) w

offerOptions :: PlayerRef -> Text -> [(Text, a)] -> WebSockets Hybi00 a
offerOptions p q opts = do
    sendText ("[Player " <> showText p <> "] " <> q)
    forM_ (zip [(1 :: Int) ..] opts) $ \(i, (opt, _)) ->
      sendText (showText i <> ") " <> opt)
    getAnswer
  where
    getAnswer = do
      input <- Text.unpack `fmap` receiveData
      case (pred `fmap` readMay input) >>= atMay opts of
        Nothing -> do
          sendText ("Invalid option" :: Text)
          getAnswer
        Just opt ->
          return (snd opt)

showText :: Show a => a -> Text
showText = Text.pack . show

sendText :: TextProtocol p => Text -> WebSockets p ()
sendText = sendTextData

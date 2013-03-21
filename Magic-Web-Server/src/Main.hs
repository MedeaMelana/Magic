{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import Magic
import Magic.Engine.Types
import Magic.BasicLands
import Magic.M13
import Magic.Json

import Control.Monad.Operational (ProgramT, ProgramViewT(..), viewT)
import Control.Monad.Random (evalRandT, newStdGen, RandT, StdGen)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (liftIO)

import Data.Aeson (encode)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import Network.WebSockets (WebSockets, Hybi00, Request, TextProtocol,
  acceptRequest, sendTextData, receiveData, runServer)


main :: IO ()
main = do
  let port = 8080
  putStrLn ("Listening on port " <> show port)
  runServer "0.0.0.0" port app

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
      instr :>>= k -> do
        let (instrJSON, getAnswer) = interactToJSON receiveData instr
        sendTextData (encode instrJSON)
        answer <- getAnswer
        askQuestions (k answer)

showText :: Show a => a -> Text
showText = Text.pack . show

sendText :: TextProtocol p => Text -> WebSockets p ()
sendText = sendTextData

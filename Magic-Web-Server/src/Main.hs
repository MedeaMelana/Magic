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

import Data.Aeson (encode)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import Network.WebSockets (PendingConnection, Connection, acceptRequest, sendTextData, receiveData, runServer)


main :: IO ()
main = do
  let port = 8080
  putStrLn ("Listening on port " <> show port)
  runServer "0.0.0.0" port app

app :: PendingConnection -> IO ()
app pending = do
  conn <- acceptRequest pending
  runFullGame conn [whiteDeck, redDeck]

redDeck :: Deck
redDeck = replicate 18 mountain <> replicate 42 searingSpear

whiteDeck :: Deck
whiteDeck = replicate 18 plains <> replicate 42 attendedKnight

runFullGame :: Connection -> [Deck] -> IO ()
runFullGame conn decks = do
  program <- evalRandTIO (evalStateT (runEngine fullGame) (newWorld decks))
  askQuestions conn program

evalRandTIO :: Monad m => RandT StdGen m a -> IO (m a)
evalRandTIO p = evalRandT p `fmap` newStdGen

askQuestions :: Connection -> ProgramT Interact (Either GameOver) () -> IO ()
askQuestions conn = eval . viewT
  where
    eval (Left gameOver) = sendTextData conn (encode gameOver)
    eval (Right program) = case program of
      Return x -> return x
      instr :>>= k -> do
        let (instrJSON, getAnswer) = interactToJSON (receiveData conn) instr
        sendTextData conn (encode instrJSON)
        answer <- getAnswer
        askQuestions conn (k answer)

showText :: Show a => a -> Text
showText = Text.pack . show

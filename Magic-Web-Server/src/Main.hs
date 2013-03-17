{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Network.WebSockets


main :: IO ()
main = runServer "0.0.0.0" 8080 app

app :: Request -> WebSockets Hybi00 ()
app req = do
  --liftIO $ Text.putStrLn ("New request: " `Text.append` Text.pack (show req))
  acceptRequest req
  meow

meow :: TextProtocol p => WebSockets p ()
meow = forever $ do
  msg <- receiveData
  --liftIO $ Text.putStrLn ("Received message: " <> msg)
  sendTextData $ msg `Text.append` ", meow."

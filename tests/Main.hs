module Main where

import Magic.Types
import Magic.BasicLands
import Magic.CLI
import Magic.M13

import Data.Monoid ((<>))

import System.IO

redDeck :: Deck
redDeck = replicate 18 mountain <> replicate 42 searingSpear

whiteDeck :: Deck
whiteDeck = replicate 18 plains <> replicate 42 attendedKnight

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runGame [redDeck, whiteDeck]

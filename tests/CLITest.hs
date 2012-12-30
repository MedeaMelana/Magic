module CLITest where

import Magic.Types
import Magic.BasicLands
import Magic.CLI
import Magic.M13

import Data.Monoid ((<>))

redDeck :: Deck
redDeck = replicate 18 mountain <> replicate 42 searingSpear

main :: IO ()
main = runGame [redDeck, redDeck]

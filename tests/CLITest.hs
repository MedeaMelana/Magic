module CLITest where

import Magic.Types
import Magic.BasicLands
import Magic.CLI

redDeck :: Deck
redDeck = replicate 60 mountain

main :: IO ()
main = runGame [redDeck, redDeck]

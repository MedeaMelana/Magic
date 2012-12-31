[![Build Status](https://secure.travis-ci.org/MedeaMelana/Magic.png?branch=master)](https://travis-ci.org/MedeaMelana/Magic)

# Magic: The Gathering in Haskell

A Haskell implementation of the rules of Wizards of the Coast's Magic: The
Gathering.

Magic is a big game. This implementation targets only a specific part of it.
For now, only two-player games and only cards, rules, card types and abilities
available and relevant in the Magic 2013 core set are targeted.

The code is currently far from finished and in very experimental state.

## Current progress

A good indication of the current progress is to open [module M13](/MedeaMelana/Magic/blob/master/Magic/M13.hs) and see how many M13 cards have been implemented yet.

There is also a command-line interface that allows you to play the game. To run it, download the source and `runghc -i.:tests CLITest`. This will run a two-player game with preselected decks.

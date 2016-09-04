[![Build Status](https://secure.travis-ci.org/MedeaMelana/Magic.png?branch=master)](https://travis-ci.org/MedeaMelana/Magic)

# Magic: The Gathering in Haskell

A Haskell implementation of the rules of Wizards of the Coast's Magic: The
Gathering.

This project has multiple goals:

* to succinctly and correctly model the interactions between Magic cards;
* to provide an elegant and correct API to express Magic cards in;
* to provide a web server that is able to run a full game where clients play against each other.

## Scope

Magic is a big game. This implementation targets only a specific part of it.
For now, only two-player games and only cards, rules, card types and abilities
available and relevant in the Magic 2013 core set are targeted.

A good indication of the current progress is to open [module M13](/Magic-Cards/src/Magic/M13.hs) and see how many M13 cards have been implemented yet. A list of issues to be fixed before the whole of M13 can be implemented can be [found on GitHub](https://github.com/MedeaMelana/Magic/milestone/1).

There is also a command-line interface that allows you to play the game. To run it, follow the installation instructions below and run the executable that it produced by building `Magic-CLI`. This will run a two-player game with preselected decks.

## Building with cabal

You need [GHC 7.8](http://www.haskell.org/ghc/download_ghc_7_8_2) or greater and [cabal-install 1.24](http://www.haskell.org/cabal/download.html) or greater to build Magic.

Clone the repository and create a Cabal sandbox:

```
$ git clone git@github.com:MedeaMelana/Magic.git
$ cd Magic
$ cabal sandbox init
```

If you want to run the web server:

```
$ cabal new-build Magic-Web-Server
$ dist-newstyle/build/Magic-Web-Server-*/build/magic-web-server/magic-web-server
```

If you want to run the command-line interface:

```
$ cabal new-build Magic-CLI
$ dist-newstyle/build/Magic-CLI-*/build/magic-cli/magic-cli
```

## Building with stack

You need the newest version of [stack](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md) to build Magic.
Clone the repository.

```
$ git clone git@github.com:MedeaMelana/Magic.git
$ cd Magic/Magic && stack build
$ cd ../../Magic/Magic-Cards && stack build
```

If you want to run the web server:

```
$ cd Magic/Magic-Web-Server
$ stack build
$ stack exec magic-web-server
```

If you want to run the command-line:

```
$ cd Magic/Magic-Cards
$ stack build
$ stack exec magic-cli
```

**Info for NixOS users**: Please use the `nix-shell` to download the newest `stack` version


## Talking to the web server

The web server runs on websockets. Currently it starts a new game for every websocket connection that is opened, and that connection has full access to all the cards in the game. That is, there is no hidden information yet, and you can't have two clients play a game against each other yet: the connecting client controls all the players in the game.

Messages to and from the server are in JSON format. There is no documentation yet about the exact form of these messages, partly because it still unstable and changes often. However, the JSON messages from the server should be reasonably clear and should contain everything you need to build a proper client.

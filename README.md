# Magic: The Gathering in Haskell

A Haskell implementation of the rules of Wizards of the Coast's Magic: The
Gathering.

Magic is a big game. This implementation targets only a specific part of it.
For now, only two-player games and only cards, rules, card types and abilities
available and relevant in the Magic 2012 core set are targeted.

The code is currently far from finished and in very experimental state.

## Todo

Short-term:

* Model everything that happens as an Event [700.1]
* Implement replacement effects [614]
* Build a monad that is able to present players with options/actions
* Build a monad that keeps track of the World before continuous effects are applied [611]
* Build a monad that offers a view of the World after continuous effects are applied [613]
* Build a monad that raises Events
* Correctly nest these monads
* Implement some cards
* Capture common card construction patterns

Long-term:

* Coin flips and die rolls
* Expand rules and cards to encompass Standard

module Magic.Object (
    -- * Objects
    Card(..), Deck,
    Object(..),
      name, colors, types, owner, controller, timestamp, counters,
      tapStatus,
      stackItem,
      pt, damage, deathtouched,
      play, staticKeywordAbilities, continuousEffects, activatedAbilities, triggeredAbilities, replacementEffects,

    -- * Object properties
    Timestamp, Color(..), TapStatus(..), CounterType(..),
  ) where

import Magic.Types

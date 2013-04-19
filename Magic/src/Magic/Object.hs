module Magic.Object (
    -- * Objects
    Card(..), Deck,
    Object(..),
      name, colors, types, owner, controller, timestamp, counters,
      tapStatus,
      stackItem,
      pt, damage, deathtouched,
      play, staticKeywordAbilities, layeredEffects, activatedAbilities, triggeredAbilities, replacementEffects,
      temporaryEffects,

    -- * Object properties
    Timestamp, Color(..), TapStatus(..), CounterType(..), PT
  ) where

import Magic.Types

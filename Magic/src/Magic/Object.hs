module Magic.Object (
    -- * Objects
    Card(..), Deck,
    Object(..),
      name, colors, types, owner, controller, timestamp, counters,
      pt, allowAttacks, allowBlocks, loyalty,
      play, alternativePlays, staticKeywordAbilities, layeredEffects, activatedAbilities, triggeredAbilities, replacementEffects,
      temporaryEffects,
    ObjectOfType(..),
      cardObject,
      permanentObject, tapStatus, damage, deathtouched, attachedTo, attacking,
      stackItemObject, stackItem,

    -- * Object properties
    Timestamp, Color(..), TapStatus(..), CounterType(..), PT
  ) where

import Magic.Types

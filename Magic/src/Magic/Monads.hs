module Magic.Monads (
    -- * Monads @ViewT@ and @View@
    ViewT(..), View, MonadView(..),

    -- * Monadic interaction with players
    Interact(..), EventSource(..), Question(..), Pick, MonadInteract(..),

    -- * Executing effects
    ExecuteEffects(..),

    -- * Monad Magic
    Magic(..)
  ) where

import Magic.Types

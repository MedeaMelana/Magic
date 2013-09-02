module Magic.World (
    -- * Data structures
    Bag,

    -- * Reference types
    PlayerRef, ObjectRef, SomeObjectRef, ActivatedAbilityRef, ZoneRef(..),
    ObjectType(..), LastKnownObjectInfo,

    -- * World
    World(..), players, activePlayer, activeStep, time, turnStructure, exile, battlefield, stack, command, turnHistory,

    -- * Turn structure
    Step(..), BeginningStep(..), CombatStep(..), EndStep(..),

    -- * Players
    Player(..), life, manaPool, prestack, library, hand, graveyard, maximumHandSize, failedCardDraw,

  ) where

import Magic.Types

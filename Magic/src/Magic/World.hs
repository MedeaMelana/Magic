module Magic.World (
    -- * Reference types
    PlayerRef, ObjectRef, SomeObjectRef, ActivatedAbilityRef, ZoneRef(..),
    ObjectType(..), LastKnownObjectInfo, toSomeObjectRef,

    -- * World
    World(..), players, activePlayer, activeStep, time, turnStructure, exile, battlefield, stack, command, turnHistory,

    -- * Turn structure
    Step(..), BeginningStep(..), CombatStep(..), EndStep(..),

    -- * Players
    Player(..), ManaEl(..), life, manaPool, prestack, library, hand, graveyard, maximumHandSize, failedCardDraw,

  ) where

import Magic.Types

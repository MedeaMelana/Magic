module Magic.Abilities (
    -- * Ability types
    Contextual,
    ActivatedAbility(..), TapCost(..),
    StackItem, ManaPool,
    StaticKeywordAbility(..),
    ReplacementEffect, TriggeredAbilities,
    PriorityAction(..), PayManaAction(..),

    -- * Constructing triggers
    onSelfETB,
  ) where

import Magic.Types



-- | Trigger whenever the source object enters the battlefield, executing the
-- argument program.
onSelfETB :: Contextual (Magic ()) -> TriggeredAbilities
onSelfETB mkProgram events rSelf p = return [ mkProgram rSelf p
  | DidMoveObject _ rOther@(Battlefield, _) <- events, rSelf == rOther ]

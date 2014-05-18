{-# LANGUAGE GADTs #-}

module Magic.Combat
  ( -- * Types
    Attack(..), Block(..)

  -- * Attacking restrictions
  -- | Restrictions that check whether a set of attacks is legal according to the current object. Use these values for a creature's 'allowAttacks' field or for further restricting such a predicate using 'RestrictAllowAttacks' in a 'LayeredEffect'.
  , selfCantAttack, selfCantAttackAlone

  -- * Blocking restrictions
-- | Restrictions that check whether a set of blocks is legal according to the current object. Use these values for a creature's 'allowBlocks' field or for further restricting such a predicate using 'RestrictAllowBlocks' in a 'LayeredEffect'.
    , selfCantBlock, selfCantBlockAlone
  , selfCantBeBlocked
  ) where

import Magic.Some
import Magic.Types

import Data.Boolean (true)
import Data.List (notElem, nub)

-- | Only allow the attacks if this creature is not part of the attacking creatures.
selfCantAttack :: [Attack] -> Contextual (View Bool)
selfCantAttack ats (Some Battlefield, i) _ =
  return ((Battlefield, i) `notElem` map attacker ats)
selfCantAttack _ _ _ = true

-- | Only allow the attacks if this creature is not part of the attacking creatures, or if there is at least one other creature attacking.
selfCantAttackAlone :: [Attack] -> Contextual (View Bool)
selfCantAttackAlone ats (Some Battlefield, i) _ =
  return ((Battlefield, i) `notElem` map attacker ats || length ats > 1)
selfCantAttackAlone _ _ _ = true

-- | Only allow the blocks if this creature is not part of the blocking creatures.
selfCantBlock :: [Block] -> Contextual (View Bool)
selfCantBlock bls (Some Battlefield, i) _ =
  return ((Battlefield, i) `notElem` map blocker bls)
selfCantBlock _ _ _ = true

-- | Only allow the blocks if this creature is not part of the blocking creatures or if at least one other creature is blocking.
selfCantBlockAlone :: [Block] -> Contextual (View Bool)
selfCantBlockAlone bls (Some Battlefield, i) _ =
    return (i `notElem` blockerIds || not (null (filter (/= i) blockerIds)))
  where
    blockerIds = nub (map (snd . blocker) bls)

-- | Only allow the blocks if this object is not part of the creatures being blocked.
selfCantBeBlocked :: [Block] -> Contextual (View Bool)
selfCantBeBlocked bls (Some Battlefield, i) _ =
  return ((Battlefield, i) `notElem` map blockee bls)
selfCantBeBlocked _ _ _ = true

module Magic.Layers (
    -- * Layered effects
    -- | Layered effects are a type of continuous effects that modify
    -- objects, players, or the game rules. Their interaction is managed by a
    -- layer system, which is why this library calls them layered effects.
    LayeredEffect(..), ModifyObject(..), Layer(..), layer,

    -- * Temporary layered effects
    TemporaryLayeredEffect(..), Duration(..)
  ) where

import Magic.Types


-- | Compute the layer in which a 'ModifyObject' applies. Object 
-- modifications are applied in order of increasing corresponding layers
-- (among other things).
layer :: ModifyObject -> Layer
layer m = case m of
  ChangeController _           -> Layer2
  ChangeTypes _                -> Layer4
  ChangeColors _               -> Layer5
  AddStaticKeywordAbility _    -> Layer6
  RemoveStaticKeywordAbility _ -> Layer6
  AddActivatedAbility _        -> Layer6
  AddTriggeredAbility _        -> Layer6
  RemoveAllAbilities           -> Layer6
  DefinePT _                   -> Layer7a
  SetPT _                      -> Layer7b
  ModifyPT _                   -> Layer7c
  SwitchPT                     -> Layer7e

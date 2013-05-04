module Magic.Layers (
    -- * Layered effects
    -- | Layered effects are a type of continuous effects that modify
    -- objects, players, or the game rules. Their interaction is managed by a
    -- layer system, which is why this library calls them layered effects.
    LayeredEffect(..), ModifyObject(..), Layer(..), layer,

    -- * Temporary layered effects
    TemporaryLayeredEffect(..), Duration(..),

    -- * Creating layered effects
    affectSelf, affectBattlefield, affectRestOfBattlefield
  ) where

import qualified Magic.IdList as IdList
import Magic.Types

import Control.Applicative ((<$>))
import Data.Label.PureM (asks)
import Data.Maybe (mapMaybe)


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
  AddTriggeredAbilities _      -> Layer6
  RemoveAllAbilities           -> Layer6
  DefinePT _                   -> Layer7a
  SetPT _                      -> Layer7b
  ModifyPT _                   -> Layer7c
  SwitchPT                     -> Layer7e


-- | Affect only the object that carries the layered effect itself.
affectSelf :: Contextual (View [ObjectRef])
affectSelf r _you = return [r]

-- | Affect objects on the battlefield, from a layered effect of an object
-- on the battlefield.
affectBattlefield ::
  (PlayerRef -> Object -> Bool) -> Contextual (View [ObjectRef])
affectBattlefield ok (Battlefield, _) you =
  mapMaybe (\(i,o) -> if ok you o then Just (Battlefield, i) else Nothing) .
    IdList.toList <$> asks battlefield
affectBattlefield _ _ _ = return []

-- | Affect all other objects on the battlefield, from a layered effect of an
-- object on the battlefield.
affectRestOfBattlefield ::
  (PlayerRef -> Object -> Bool) -> Contextual (View [ObjectRef])
affectRestOfBattlefield ok (Battlefield, iSelf) you =
  mapMaybe (\(i,o) -> if iSelf /= i && ok you o then Just (Battlefield, i) else Nothing) .
    IdList.toList <$> asks battlefield
affectRestOfBattlefield _ _ _ = return []

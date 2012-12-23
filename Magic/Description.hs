{-# LANGUAGE OverloadedStrings #-}

module Magic.Description where

import Magic.Core
import Magic.Types
import Magic.IdList (Id, toList)
import Magic.Labels

import Prelude hiding (unlines)

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (ask)

import Data.Label.Pure (get)
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.Text (Text, pack)
import qualified Data.Text as Text



-- TYPE DESCRIPTION


newtype Description = Description { runDescription :: View Text }

instance IsString Description where
  fromString = Description . return . pack

instance Monoid Description where
  mempty = Description (return mempty)
  Description x `mappend` Description y = Description (mappend <$> x <*> y)

text :: Text -> Description
text = Description . return

sh :: Show a => a -> Description
sh = text . pack . show

withWorld :: (World -> Description) -> Description
withWorld f = Description (ask >>= runDescription . f)



-- UTILITY FUNCTIONS


intercalate :: Description -> [Description] -> Description
intercalate x ys = Description (Text.intercalate <$> runDescription x <*> mapM runDescription ys)

unlines :: [Description] -> Description
unlines xs = Description (Text.unlines <$> mapM runDescription xs)



-- DESCRIPTIONS


describePriorityAction :: PriorityAction -> Description
describePriorityAction a =
  case a of
    PlayCard ro -> "Play " <> sh ro
    ActivateAbility ro i -> "Activate ability " <> sh i <> " of " <> sh ro

describeWorld :: Description
describeWorld = withWorld $ \world -> unlines
  [ "Player " <> sh (get activePlayer world) <> "'s turn"
  , sh (get activeStep world)
  ]

describeHand :: PlayerRef -> Description
describeHand p = withWorld $ \world -> intercalate ", " $
  map describeObject (toList (get (player p .^ hand) world))

describeObject :: (Id, Object) -> Description
describeObject (i, o) =
  case get name o of
    Just n -> "#" <> sh i <> " " <> text n
    Nothing -> "#" <> sh i

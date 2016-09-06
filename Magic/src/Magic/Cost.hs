{-# LANGUAGE TypeOperators #-}

module Magic.Cost (
    ManaCost, ManaCostEl(..),
    white, blue, black, red, green, colorless, generic,
    cmc
  ) where

import Magic.Types

import Data.Label (set, (:->))
import Data.Monoid (Monoid(..), (<>))
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

white :: Int -> ManaCost
white = mkColorCost White

blue :: Int -> ManaCost
blue = mkColorCost Blue

black :: Int -> ManaCost
black = mkColorCost Black

red :: Int -> ManaCost
red = mkColorCost Red

green :: Int -> ManaCost
green = mkColorCost Green

mkColorCost :: Color -> Int -> ManaCost
mkColorCost = mkManaCost . ManaElCost . ColorEl

colorless :: Int -> ManaCost
colorless = mkManaCost (ManaElCost (ColorlessEl))

generic :: Int -> ManaCost
generic = mkManaCost GenericCost

mkManaCost :: ManaCostEl -> Int -> ManaCost
mkManaCost x n 
  | n >= 0     = MultiSet.insertMany x n MultiSet.empty
  | otherwise  = error ("negative mana cost: " <> show n)

cmc :: ManaCost -> Int
cmc = MultiSet.size

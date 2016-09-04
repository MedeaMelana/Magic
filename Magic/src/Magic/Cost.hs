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
white = mkManaCost (ColorCost White)

blue :: Int -> ManaCost
blue = mkManaCost (ColorCost Blue)

black :: Int -> ManaCost
black = mkManaCost (ColorCost Black)

red :: Int -> ManaCost
red = mkManaCost (ColorCost Red)

green :: Int -> ManaCost
green = mkManaCost (ColorCost Green)

colorless :: Int -> ManaCost
colorless = mkManaCost ColorlessCost

generic :: Int -> ManaCost
generic = mkManaCost GenericCost

mkManaCost :: ManaCostEl -> Int -> ManaCost
mkManaCost x n 
  | n >= 0     = MultiSet.insertMany x n MultiSet.empty
  | otherwise  = error ("negative mana cost: " <> show n)

cmc :: ManaCost -> Int
cmc = MultiSet.size

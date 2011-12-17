{-# LANGUAGE TypeOperators #-}

module Utils where

import Types
import Labels

import Data.Label.Maybe ((:~>))

object :: Ref Object -> World :~> Object
object r = objects .^ ref r

player :: Ref Player -> World :~> Player
player r = players .^ ref r

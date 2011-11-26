{-# LANGUAGE OverloadedStrings #-}

module Innistrad where

import Magic

shock :: Card
shock = Card
  { name = "Shock"
  , manaCost = [Just Red]
  , rarity = Common
  , group = Spell Instant
  }

module Strategies
  ( constrain
  ) where

import Strategies.Internal
import Board


strats :: [Board -> Board]
strats = [opOnGroups removeSolved, opOnGroups fixSingles]

constrain :: Board -> Board
constrain board = if board == updated then board else constrain updated
  where updated = foldr (.) id strats $ board
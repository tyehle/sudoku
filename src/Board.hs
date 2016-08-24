module Board
  ( Board(..)
  , constrain
  ) where

import Board.Internal (Board(..), opOnGroups, removeExisting, fixSingles)


constrain :: Board -> Board
constrain board = if board == updated then board else constrain updated
  where updated = (opOnGroups removeExisting) $ (opOnGroups fixSingles) $ board
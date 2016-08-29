module Strategies
  ( constrain
  ) where

import Strategies.Internal
import Board


constrain :: Board -> Board
constrain = execStrats [opOnGroups removeSolved, opOnGroups fixSingles]



execStrats :: [Board -> Board] -> Board -> Board
execStrats strats input = case bindAll input (map failWhenChanged strats) of
  Left changed -> execStrats strats changed
  Right done -> done

bindAll :: (Foldable t, Monad m) => a -> t (a -> m a) -> m a
bindAll start cs = foldr (=<<) (return start) cs

failWhenChanged :: (Board -> Board) -> Board -> Either Board Board
failWhenChanged fn input | changed == input = Right input
                         | otherwise        = Left changed
  where
    changed = fn input

module Strategies
  ( constrain
  ) where

import Strategies.Internal
import Board


constrain :: Board -> Board
constrain = execStrats [removeSolved, fixSingles, singleBox]



execStrats :: [Board -> Board] -> Board -> Board
execStrats strats input = case composeM input (map failWhenChanged strats) of
  Left changed -> execStrats strats changed
  Right done -> done

composeM :: (Foldable t, Monad m) => a -> t (a -> m a) -> m a
composeM = foldr (=<<) . return

failWhenChanged :: (Board -> Board) -> Board -> Either Board Board
failWhenChanged fn input | changed == input = Right input
                         | otherwise        = Left changed
  where
    changed = fn input

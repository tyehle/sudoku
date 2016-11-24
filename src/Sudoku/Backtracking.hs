module Sudoku.Backtracking where

import Sudoku.Board
import Sudoku.Strategies

import System.Random
import Control.Monad.State.Lazy (State, state, evalState)
import Data.List (foldl', delete)

solve :: StdGen -> Board -> Maybe Board
solve rng board = evalState (solver board) rng

solver :: Board -> State StdGen (Maybe Board)
solver board | isSolved constrained        = return $ Just constrained
             | not . isValid $ constrained = return Nothing
             | otherwise = do
                  chosenCell <- choice $ mostConstrainedCells constrained
                  chosenValue <- choice $ getCell constrained chosenCell
                  with <- solver $ modify (const [chosenValue]) constrained chosenCell
                  case with of
                    Nothing -> solver $ modify (delete chosenValue) constrained chosenCell
                    Just solved -> return with
  where
    constrained = constrain board



mostConstrainedCells :: Board -> [Position]
mostConstrainedCells board@(Board m n contents) = collectMostConstrained board unknownPoss (m*n) []
  where
    unknownPoss = filter ((> 1) . length . getCell board) $ concat $ rows m n

collectMostConstrained :: Board -> [Position] -> Int -> [Position] -> [Position]
collectMostConstrained _ [] _ found = found
collectMostConstrained board (pos:others) size found | currentSize < size  = collectMostConstrained board others currentSize [pos]
                                                     | currentSize == size = collectMostConstrained board others size (pos:found)
                                                     | otherwise           = collectMostConstrained board others size found
  where
    currentSize = length $ getCell board pos



choice :: [a] -> State StdGen a
choice []    = error "cannot choose from an empty list"
choice items = fmap (items !!) . dieRoll . length $ items


dieRoll :: Int -> State StdGen Int
dieRoll upper | upper < 1 = return 0
              | otherwise = state $ randomR (0,upper-1)

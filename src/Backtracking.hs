module Backtracking where

import Board
import Strategies

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
                    Just solved -> return $ Just solved
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


branch :: Board -> Position -> Int -> (Board, Board)
branch board pos value = (with, without)
  where
    with = modify (const [value]) board pos
    without = modify (delete value) board pos


choice :: [a] -> State StdGen a
choice items = fmap ((items !!) . (subtract 1)) $ dieRoll $ length items


dieRoll :: Int -> State StdGen Int
dieRoll upper = state $ randomR (1,upper)
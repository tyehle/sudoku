module Strategies.Internal where

import Data.List (group, foldl', (\\), sort, intersect)

import Board

opOnGroups :: (Board -> [Position] -> Board) -> Board -> Board
opOnGroups op board@(Board m n contents) = doGroup (rows m n) $ doGroup (cols m n) $ doGroup (boxes m n) board
  where
    doGroup poss b = foldl' op b poss


singlesOnly :: [a] -> [a]
singlesOnly xs | length xs == 1 = xs
               | otherwise      = []



removeSolved :: Board -> [Position] -> Board
removeSolved board cells = foldl' (modify (\\ defined)) board unknownPositions
  where
    defined = map (getCell board) cells >>= singlesOnly
    unknownPositions = filter (\c -> length (getCell board c) > 1) cells



fixSingles :: Board -> [Position] -> Board
fixSingles board cells = foldl' (modify (maybeFix toFix)) board unknownPositions
  where
    unknownPositions = filter (\c -> length (getCell board c) > 1) cells
    toFix = (group . sort . concat) (map (getCell board) cells) >>= singlesOnly

maybeFix :: [Int] -> [Int] -> [Int]
maybeFix toFix current | length intersection > 0 = intersection
                       | otherwise               = current
  where
    intersection = intersect current toFix
module Strategies.Internal where

import Debug.Trace (trace)
import Data.List (group, foldl', (\\), sort, intersect)

import Board


opOnGroups :: (Board -> [Position] -> Board) -> [Int -> Int -> [[Position]]] -> Board -> Board
opOnGroups op groups board@(Board m n _) = foldl' doGroup board groups
  where
    doGroup b group = foldl' op b (group m n)


singlesOnly :: [a] -> [a]
singlesOnly xs | length xs == 1 = xs
               | otherwise      = []


goodNub :: Ord a => [a] -> [a]
goodNub [] = []
goodNub xs = head . group . sort $ xs



removeSolved :: Board -> Board
removeSolved = opOnGroups removeSolvedGroup [rows, cols, boxes]

removeSolvedGroup :: Board -> [Position] -> Board
removeSolvedGroup board cells = foldl' (modify (\\ defined)) board unknownPositions
  where
    defined = map (getCell board) cells >>= singlesOnly
    unknownPositions = filter ((> 1) . length . getCell board) cells



fixSingles :: Board -> Board
fixSingles = opOnGroups fixSinglesGroup [rows, cols, boxes]

fixSinglesGroup :: Board -> [Position] -> Board
fixSinglesGroup board cells = foldl' (modify (maybeFix toFix)) board unknownPositions
  where
    unknownPositions = filter ((> 1) . length . getCell board) cells
    toFix = (group . sort . concat) (map (getCell board) cells) >>= singlesOnly

maybeFix :: [Int] -> [Int] -> [Int]
maybeFix toFix current | length intersection > 0 = intersection
                       | otherwise               = current
  where
    intersection = intersect current toFix



singleBox :: Board -> Board
singleBox = opOnGroups singleBoxLine [rows]--, cols]

singleBoxLine :: Board -> [Position] -> Board
singleBoxLine board@(Board m n _) line = foldl' runBox board boxesInLine
  where
    boxesInLine = filter (not . null . (intersect line)) (boxes m n)
    runBox b box = singleBoxGroups box line $ singleBoxGroups line box b

singleBoxGroups :: [Position] -> [Position] -> Board -> Board
singleBoxGroups allGroup removeGroup board = foldl' (modify (\\ canRemove)) board removeGroupOthersUnknown
  where
    both = allGroup `intersect` removeGroup
    allGroupOthers = allGroup \\ removeGroup
    removeGroupOthersUnknown = filter ((/= 1) . length . getCell board) $ removeGroup \\ allGroup
    unknownIn = concat . filter ((/= 1) . length) . map (getCell board)
    canRemove = (goodNub . unknownIn) both \\ unknownIn allGroupOthers

module Sudoku.Strategies.Internal where

import Data.List (group, foldl', (\\), sort, intersect)

import Sudoku.Board


opOnGroups :: (Board -> [Position] -> Board) -> [Int -> Int -> [[Position]]] -> Board -> Board
opOnGroups op groups board@(Board m n _) = foldl' doGroup board groups
  where
    doGroup b group = foldl' op b (group m n)


singlesOnly :: [a] -> [a]
singlesOnly xs | length xs == 1 = xs
               | otherwise      = []


goodNub :: Ord a => [a] -> [a]
goodNub = map head . group . sort



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
maybeFix toFix current | not $ null intersection = intersection
                       | otherwise               = current
  where
    intersection = current `intersect` toFix



singleBox :: Board -> Board
singleBox = opOnGroups singleBoxLine [rows, cols]

singleBoxLine :: Board -> [Position] -> Board
singleBoxLine board@(Board m n _) line = foldl' runBox board boxesInLine
  where
    boxesInLine = filter (not . null . intersect line) $ boxes m n
    runBox b box = singleBoxGroups box line $ singleBoxGroups line box b

singleBoxGroups :: [Position] -> [Position] -> Board -> Board
singleBoxGroups allGroup removeGroup board = foldl' (modify (\\ canRemove)) board removeGroupOthersUnknown
  where
    removeGroupOthersUnknown = filter ((/= 1) . length . getCell board) $ removeGroup \\ allGroup
    both = allGroup `intersect` removeGroup
    uniqueUnknownInBoth = goodNub . concat . filter ((/= 1) . length) . map (getCell board) $ both
    allGroupOthers = allGroup \\ removeGroup
    allGroupOthersContents = concatMap (getCell board) allGroupOthers
    canRemove = uniqueUnknownInBoth \\ allGroupOthersContents



disjointSubset :: Board -> Board
disjointSubset = opOnGroups disjointSubsetGroup [rows, cols, boxes]

disjointSubsetGroup :: Board -> [Position] -> Board
disjointSubsetGroup board group = foldl' (modify updateCell) board group
  where
    unknownDisjointSubsets = filter ((> 1) . length) $ findDisjointSets (map (getCell board) group)
    updateCell cell = foldl' removePossibilities cell unknownDisjointSubsets
    removePossibilities cell subset | cell == subset = cell
                                    | otherwise      = cell \\ subset

findDisjointSets :: Ord a => [[a]] -> [[a]]
findDisjointSets cells = map head . filter (\equalCells -> length equalCells == (length . head) equalCells) $ equalGroups
  where
    equalGroups = group . sort $ map sort cells



disjointChain :: Board -> Board
disjointChain = opOnGroups disjointChainGroup [rows, cols, boxes]

disjointChainGroup :: Board -> [Position] -> Board
disjointChainGroup board group = foldl' (removeChains chains) board unknownPositions
  where
    unknownPositions = filter ((> 1) . length . getCell board) group
    possibleChains = filter ((> 1) . length) $ powerset unknownPositions
    chains = filter (\chain -> length chain == (length . goodNub . map (getCell board)) chain) possibleChains

removeChains :: [[Position]] -> Board -> Position -> Board
removeChains chains board cell = modify (\\ numbersToRemove) board cell
  where
    chainsToRemove = filter (notElem cell) chains
    numbersToRemove = goodNub $ concatMap (getCell board) $ concat chainsToRemove

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

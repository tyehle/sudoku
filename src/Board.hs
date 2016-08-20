module Board
  ( Board(..), Position
  , padString, groupN
  , modify, emptyBoard
  , fix, toIndex, isValid
  , constrain, removeExisting, fixSingles
  , rows, cols, boxes
  ) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate, (\\))
import Data.Foldable (toList)



-- (row, col)
type Position = (Int, Int)

data Board = Board Int Int (Seq (Set Int)) deriving (Eq)

instance Show Board where
  show (Board m n contents) = showBoard m n (map showOptions (toList contents))


padString :: Int -> String -> String
padString width s | even toAdd = concat [halfSpace, s, halfSpace]
                  | otherwise  = concat [halfSpace, s, " ", halfSpace]
  where
    toAdd = width - (length s)
    halfSpace = replicate (toAdd `quot` 2) ' '

showOptions :: Set Int -> String
showOptions options = intercalate "," (map show (Set.toList options))

groupN :: Int -> [a] -> [[a]]
groupN n xs | n < 1          = []
            | null xs        = []
            | length xs <= n = [xs]
            | otherwise      = take n xs : groupN n (drop n xs)


showBoard :: Int -> Int -> [String] -> String
showBoard m n contents = concat [top, "\n", blank, "\n│  ", bandDividors, "  │\n", blank, "\n", bottom]
  where
    hSpan fill l c r = concat [l, intercalate c $ replicate n (replicate (m*(longest+2)+2) fill), r]
    top    = hSpan '─' "┌" "┬" "┐"
    middle = hSpan '─' "├" "┼" "┤"
    bottom = hSpan '─' "└" "┴" "┘"
    blank  = hSpan ' ' "│" "│" "│"
    longest = maximum $ map length contents
    padded = map (padString longest) contents
    cellDividors = map (intercalate "  ") $ groupN m padded
    stackDividors = map (intercalate "  │  ") $ groupN n cellDividors
    rowDividors = map (intercalate (concat ["  │\n", blank, "\n│  "])) $ groupN n stackDividors
    bandDividors = intercalate (concat ["  │\n", blank, "\n", middle, "\n",blank, "\n│  "]) rowDividors



emptyBoard :: Board
emptyBoard = Board 3 3 (Seq.replicate (9*9) (Set.fromList [1..9]))



modify :: Int -> (Set Int -> Set Int) -> Board -> Board
modify pos updater (Board m n contents) = Board m n (Seq.update pos updated contents)
  where
    updated = updater (Seq.index contents pos)

fix :: Int -> Int -> Board -> Board
fix pos value board = modify pos (const (Set.singleton value)) board

toIndex :: Int -> Int -> Position -> Int
toIndex m n (row, col) = (m*n)*row + col

isValid :: Board -> Bool
isValid board = any null constrained
  where
    Board _ _ constrained = constrain board

without :: [Int] -> [Int] -> [Int]
without elems remove = elems \\ remove



constrain :: Board -> Board
constrain board = if board == updated then board else constrain updated
  where updated = removeExisting . fixSingles $ board

removeExisting :: Board -> Board
removeExisting (Board m n contents) = Board m n contents
  where
    ind = toIndex m n
    changedRows = "not done"

fixSingles :: Board -> Board
fixSingles board = board



rows :: Int -> Int -> [[Position]]
rows m n = map (\col -> map (\row -> (row, col)) indices) indices
  where indices = [0..m*n-1]

cols :: Int -> Int -> [[Position]]
cols m n = map (\row -> map (\col -> (row, col)) indices) indices
  where indices = [0..m*n-1]

boxes :: Int -> Int -> [[Position]]
boxes m n = map boxFromCorner corners
  where
    indicesN = [0..n-1]
    indicesM = [0..m-1]
    corners = concat $ map (\col -> map (\row -> (row*n, col*m)) indicesM) indicesN
    boxFromCorner (y, x) = concat $ map (\col -> map (\row -> (row + y, col + x)) indicesN) indicesM
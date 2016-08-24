module Board where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.List (intercalate, (\\), foldl', group, sort, intersect)
import Data.Foldable (toList)


data Board = Board Int Int (Seq [Int]) deriving (Eq)

empty :: Int -> Int -> Board
empty m n = Board m n (Seq.replicate (m*m*n*n) [1..(m*n)])

-- (row, col)
type Position = (Int, Int)

instance Show Board where
  show (Board m n contents) = showBoard m n (map showOptions (toList contents))


padString :: Int -> String -> String
padString width s | even toAdd = concat [halfSpace, s, halfSpace]
                  | otherwise  = concat [halfSpace, s, " ", halfSpace]
  where
    toAdd = width - (length s)
    halfSpace = replicate (toAdd `quot` 2) ' '

showOptions :: [Int] -> String
showOptions options = intercalate "," (map show options)

groupN :: Int -> [a] -> [[a]]
groupN n xs | n < 1          = []
            | null xs        = []
            | length xs <= n = [xs]
            | otherwise      = take n xs : groupN n (drop n xs)


showBoard :: Int -> Int -> [String] -> String
showBoard 0 0 [] = "┌┐\n││\n└┘"
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




modify :: ([Int] -> [Int]) -> Board -> Position -> Board
modify updater (Board m n contents) pos = Board m n (Seq.update index updated contents)
  where
    index = toIndex m n pos
    updated = updater (Seq.index contents index)

maybeModify :: Maybe [Int] -> Board -> Position -> Board
maybeModify (Just new) (Board m n contents) pos = Board m n (Seq.update (toIndex m n pos) new contents)
maybeModify Nothing board _ = board

toIndex :: Int -> Int -> Position -> Int
toIndex m n (row, col) = (m*n)*row + col

getCell :: Board -> Position -> [Int]
getCell (Board m n contents) pos = Seq.index contents $ toIndex m n pos

-- isValid :: Board -> Bool
-- isValid board = any null constrained
--   where
--     Board _ _ constrained = constrain board



constrain :: Board -> Board
constrain board = if board == updated then board else constrain updated
  where updated = (opOnGroups removeExisting) $ (opOnGroups fixSingles) $ board


opOnGroups :: (Board -> [Position] -> Board) -> Board -> Board
opOnGroups op board@(Board m n contents) = doGroup (rows m n) $ doGroup (cols m n) $ doGroup (boxes m n) board
  where
    doGroup poss b = foldl' op b poss


singlesOnly :: [a] -> [a]
singlesOnly xs | length xs == 1 = xs
               | otherwise      = []

removeExisting :: Board -> [Position] -> Board
removeExisting board cells = foldl' (modify (\\ defined)) board unknownPositions
  where
    defined = map (getCell board) cells >>= singlesOnly
    unknownPositions = filter (\c -> length (getCell board c) > 1) cells

tryFilterExisting :: [Int] -> Board -> Position -> Board
tryFilterExisting defined = modify (\\ defined)


fixSingles :: Board -> [Position] -> Board
fixSingles board cells = board --foldr (\pos -> \b -> maybeModify (tryFix toFix b pos) board pos) board unknownPositions
  where
    unknownPositions = filter (\c -> length (getCell board c) > 1) cells
    toFix = (group . sort . concat) (map (getCell board) cells) >>= singlesOnly

tryFix :: [Int] -> Board -> Position -> Maybe [Int]
tryFix toFix board pos | length intersection > 0 = Just intersection
                       | otherwise               = Nothing
  where
    intersection = intersect (getCell board pos) toFix



rows :: Int -> Int -> [[Position]]
rows m n = map (\row -> map (\col -> (row, col)) indices) indices
  where indices = [0..m*n-1]

cols :: Int -> Int -> [[Position]]
cols m n = map (\col -> map (\row -> (row, col)) indices) indices
  where indices = [0..m*n-1]

boxes :: Int -> Int -> [[Position]]
boxes m n = map boxFromCorner corners
  where
    indicesN = [0..n-1]
    indicesM = [0..m-1]
    corners = concat $ map (\row -> map (\col -> (row*n, col*m)) indicesN) indicesM
    boxFromCorner (y, x) = concat $ map (\row -> map (\col -> (row + y, col + x)) indicesM) indicesN
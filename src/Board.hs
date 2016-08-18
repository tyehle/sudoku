module Board
  ( Board(..)
  , modify, emptyBoard
  , rows, cols, boxes
  ) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq



-- (row, col)
type Position = (Int, Int)

data Board = Board Int Int (Seq [Int]) deriving (Eq, Show)



emptyBoard :: Board
emptyBoard = Board 3 3 (Seq.replicate (9*9) [1..9])



modify :: Int -> ([Int] -> [Int]) -> Board -> Board
modify pos updater (Board m n contents) = Board m n (Seq.update pos updated contents)
  where
    updated = updater (Seq.index contents pos)

fix :: Int -> Int -> Board -> Board
fix pos value board = modify pos (const [value]) board

toIndex :: Int -> Int -> Position -> Int
toIndex m n (row, col) = (m*n)*row + col

isValid :: Board -> Bool
isValid board = any null constrained
  where
    Board _ _ constrained = constrain board



constrain :: Board -> Board
constrain board = if board == updated then board else constrain updated
  where updated = removeExisting . fixSingles $ board

removeExisting :: Board -> Board
removeExisting board = board

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
module Main
where

import System.IO
import System.Random

import Sudoku.Parser
import Sudoku.Board
import Sudoku.Backtracking

main :: IO ()
main = do
  handle <- openFile "board-4x4-hard.txt" ReadMode
  input <- hGetContents handle
  rng <- getStdGen
  putStrLn $ case parseBoard input of
    Left err -> show err
    Right b -> maybe "not solvable" prettyPrint $ solve rng b
  hClose handle

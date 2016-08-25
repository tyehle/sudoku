module Main
where

import System.IO

import Parser
import Board
import Strategies

main :: IO ()
main = do
  handle <- openFile "board-4x4.txt" ReadMode
  input <- hGetContents handle
  putStrLn $ case parseBoard input of
    Left err -> show err
    Right b -> show . constrain $ b
  hClose handle


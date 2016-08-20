module Main
where

import Parser
-- import Board (Board)
import System.IO

main :: IO ()
main = do
  handle <- openFile "board-3x2.txt" ReadMode
  input <- hGetContents handle
  putStrLn $ case parseBoard input of
    Left err -> show err
    Right b -> show b
  hClose handle


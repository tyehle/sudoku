module Main
where

import Parser
import Board (Board, constrain)
import System.IO

main :: IO ()
main = do
  handle <- openFile "board-3x3-hard.txt" ReadMode
  input <- hGetContents handle
  putStrLn $ case parseBoard input of
    Left err -> show err
    Right b -> show . constrain $ b
  hClose handle


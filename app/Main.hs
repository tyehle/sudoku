module Main
where

import Parser
import Board (Board, constrain)
import System.IO

main :: IO ()
main = do
  handle <- openFile "board-4x4.txt" ReadMode
  input <- hGetContents handle
  putStrLn $ case parseBoard input of
    Left err -> show err
    Right b -> show . constrain $ b
  hClose handle


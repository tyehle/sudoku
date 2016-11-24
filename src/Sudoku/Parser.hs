module Sudoku.Parser
  ( parseBoard
  ) where

import Sudoku.Board (Board)
import Sudoku.Parser.Internal (board)
import Text.Parsec (ParseError, runParser)

parseBoard :: String -> Either ParseError Board
parseBoard = runParser board 0 "parameter"

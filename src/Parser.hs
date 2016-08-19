module Parser
  ( parseBoard
  ) where

import Board (Board)
import Parser.Internal (board)
import Text.Parsec (ParseError, runParser)

parseBoard :: String -> Either ParseError Board
parseBoard = runParser board 0 "parameter"
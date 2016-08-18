module Parser
  ( parseBoard
  ) where

import Data.Char (digitToInt)
import Board (Board(..))
import Text.Parsec
import Data.Sequence (Seq, fromList)


type Parser a = Parsec String Int a



sepByN :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepByN n p sep  | n <= 0    = return []
                | otherwise = do
                    x <- p
                    xs <- count (n-1) (sep >> p)
                    return (x:xs)

sepEndByN :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndByN n p sep = sepByN n p sep <* optional sep



dimension :: Parser (Int, Int)
dimension = do
  m <- digit >>= return . digitToInt
  spaces
  n <- digit >>= return . digitToInt
  spaces
  putState (m*n)
  return (m, n)

entry :: Parser [Int]
entry =  (digit >>= return . return . digitToInt)
     <|> (char '_' >> getState >>= return . enumFromTo 1)

entries :: Parser (Seq [Int])
entries = getState >>= \total -> sepEndByN (total*total) entry spaces >>= return . fromList

board :: Parser Board
board = do
  spaces
  (m, n) <- dimension
  cells <- entries
  return $ Board m n cells



parseBoard :: String -> Either ParseError Board
parseBoard = runParser board 0 "parameter"
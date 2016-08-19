module Parser.Internal
  ( sepByN, sepEndByN
  , number
  , dimension, entry, entries
  , board
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



number :: Parser Int
number = fmap read (many1 digit)

dimension :: Parser (Int, Int)
dimension = do
  m <- number
  spaces
  n <- number
  spaces
  putState (m*n)
  return (m, n)

entry :: Parser [Int]
entry =  (fmap return number)
     <|> (char '_' >> fmap (enumFromTo 1) getState)

entries :: Parser (Seq [Int])
entries = getState >>= \total -> fmap fromList $ sepEndByN (total*total) entry spaces

board :: Parser Board
board = do
  spaces
  (m, n) <- dimension
  cells <- entries
  eof
  return $ Board m n cells
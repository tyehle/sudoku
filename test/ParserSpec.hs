module ParserSpec
  ( parserTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import Parser.Internal
import Board (Board(..))

import Text.Parsec
import Data.Sequence (fromList)


type Parser a = Parsec String Int a

testParse :: Parser a -> String -> Maybe a
testParse p input = case runParser p 0 "test string" input of
  Right result -> Just result
  Left _ -> Nothing



parserTests = testGroup "parser tests"
  [ sepByNTests, sepEndByNTests
  , numberTests
  , dimensionTests
  , entryTests, entriesTests
  , boardTests, parseBoardTests
  ]



sepByNTests = testGroup "sepByN"
  [ testCase "empty" $ testParse (sepByN 0 digit spaces) "2  3" @?= Just ""
  , testCase "single" $ testParse (sepByN 1 digit spaces) "2  3" @?= Just "2"
  , testCase "end of input" $ testParse (sepByN 2 digit spaces) "2 3" @?= Just "23"
  , testCase "too many" $ testParse (sepByN 3 digit spaces) "2  3" @?= Nothing
  ]

sepEndByNTests = testGroup "sepEndByN"
  [ testCase "empty" $ testParse (sepEndByN 0 digit spaces) "2  3" @?= Just ""
  , testCase "eat end" $ testParse (sepEndByN 1 digit spaces >> getInput) "2  3asdf" @?= Just "3asdf"
  , testCase "no trailing" $ testParse (sepEndByN 1 digit spaces >> getInput) "2" @?= Just ""
  , testCase "single" $ testParse (sepEndByN 1 digit spaces) "2  3" @?= Just "2"
  , testCase "too many" $ testParse (sepEndByN 3 digit spaces) "2  3" @?= Nothing
  ]

numberTests = testGroup "number"
  [ testCase "empty" $ testParse number "" @?= Nothing
  , testCase "one digit" $ testParse number "9" @?= Just 9
  , testCase "three digits" $ testParse number "285" @?= Just 285
  , testCase "trailing" $ testParse (number >> getInput) "12 3" @?= Just " 3"
  ]

dimensionTests = testGroup "dimension"
  [ testCase "eat end" $ testParse (dimension >> getInput) "2  3\nasdf" @?= Just "asdf"
  , testCase "multiple digits" $ testParse dimension "12 4 " @?= Just (12, 4)
  , testCase "state" $ testParse (dimension >> getState) "3 5" @?= Just 15
  ]

entryTests = testGroup "entry"
  [ testCase "blank" $ testParse (putState 6 >> entry) "_" @?= Just [1..6]
  , testCase "set" $ testParse entry "12asdf" @?= Just [12]
  , testCase "set remaining" $ testParse (entry >> getInput) "12asdf" @?= Just "asdf"
  ]

entriesTests = testGroup "entries"
  [ testCase "trailing" $ testParse (putState 2 >> entries >> getInput) "1 2\n_ _ other" @?= Just "other"
  , testCase "too few" $ testParse (putState 6 >> entries) "1 2 _ _ 4" @?= Nothing
  , testCase "just right" $ testParse (putState 2 >> entries) "1 2 _ _" @?= Just (fromList [[1], [2], [1,2], [1,2]])
  ]

boardTests = testGroup "board"
  [ testCase "trailing" $ testParse board "1 2\n1 _\n2 1 " @?= Just (Board 1 2 (fromList [[1], [1,2], [2], [1]]))
  , testCase "trailing bad" $ testParse board "1 2\n1 _\n2 1 other" @?= Nothing
  , testCase "preceding" $ testParse board "   1 2\n1 _\n2 1 " @?= Just (Board 1 2 (fromList [[1], [1,2], [2], [1]]))
  ]

succeeded :: Either f s -> Bool
succeeded = either (const False) (const True)

parseBoardTests = testGroup "parseBoard"
  [ testCase "good" $ succeeded (parseBoard "   1 2\n1 _\n2 1 ") @?= True
  , testCase "bad" $ succeeded (parseBoard "   1 2\n1 _\n2 1 3 ") @?= False
  ]
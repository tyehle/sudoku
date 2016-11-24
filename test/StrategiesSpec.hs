module StrategiesSpec
  ( strategiesTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Sequence as Seq
import Data.List (foldl')

import Sudoku.Board
import Sudoku.Parser(parseBoard)
import Sudoku.Strategies
import Sudoku.Strategies.Internal

strategiesTests = testGroup "board tests"
  [ opOnGroupsTests
  , singlesOnlyTests
  , removeSolvedTests
  , fixSinglesTests
  , maybeFixTests
  , singleBoxGroupsTests
  , singleBoxLineTests
  , singleBoxTests
  , disjointSubsetTests
  ]



prependIndices :: Board -> [Position] -> Board
prependIndices board@(Board m n contents) poss = foldl' prependIndex board (zip poss [0..])
  where
    prependIndex b (pos, i) = modify (i:) b pos



opOnGroupsTests = testGroup "opOnGroups"
  [ testCase "prepend index" $ opOnGroups prependIndices [rows, cols, boxes] (Board 1 2 (Seq.fromList [[], [], [], []])) @?= Board 1 2 (Seq.fromList [[0, 0, 0], [0, 0, 1], [1, 1, 0], [1, 1, 1]])
  ]

singlesOnlyTests = testGroup "singlesOnly"
  [ testCase "single" $ singlesOnly [1] @?= [1]
  , testCase "zero" $ singlesOnly [] @?= ([] :: [Int])
  , testCase "three" $ singlesOnly [1,2,3] @?= []
  ]

removeSolvedTests = testGroup "removeSolved"
  [ testCase "in order" $ removeSolvedGroup (Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [1, 2]])) [(0, 0), (0, 1)] @?= Board 1 2 (Seq.fromList [[1], [2], [2, 1], [1, 2]])
  , testCase "out of order" $ removeSolvedGroup (Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [2, 1]])) [(0, 1), (1, 1)] @?= Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [1]])
  ]

fixSinglesTests = testGroup "fixSingles"
  [ testCase "in order" $ fixSinglesGroup (Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [1, 2]])) [(0, 0), (0, 1)] @?= Board 1 2 (Seq.fromList [[1], [2], [2, 1], [1, 2]])
  , testCase "out of order" $ fixSinglesGroup (Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [2, 1]])) [(0, 1), (1, 1)] @?= Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [1]])
  ]

maybeFixTests = testGroup "maybeFix"
  [ testCase "empty toFix" $ maybeFix [] [1,2,3] @?= [1,2,3]
  , testCase "empty current" $ maybeFix [1,2] [] @?= []
  , testCase "single match" $ maybeFix [1] [2, 1, 3] @?= [1]
  , testCase "no match" $ maybeFix [1] [2,3,4] @?= [2,3,4]
  , testCase "double match" $ maybeFix [2,1] [3,1,2,4] @?= [1,2]
  ]


singleBoxBoard = Board 3 2 (Seq.fromList [ [1,2], [3], [1,4], [5], [6], [2,4]
                                         , [1,2,6], [5], [1,4,6], [1,4,3], [1,2,3], [2,4]
                                         , 1:[3..6], [1..6], [1,2,3,5,6], [2], [4], [1]
                                         , [1,4], [1,2,4], [1,2], [1,2,3,4,6], [1..5], [3]
                                         , [4], [1..6], [1,2,3,5,6], [1..4], [1..4], [1..6]
                                         , 1:[3..6], [1..6], [2], [1..4], [1..4], [1..6]])

singleBoxBoardConstrained = Board 3 2 (Seq.fromList [ [1,2], [3], [1,4], [5], [6], [2,4]
                                                    , [2,6], [5], [4,6], [1,4,3], [1,2,3], [2,4]
                                                    , 1:[3..6], [1..6], [1,2,3,5,6], [2], [4], [1]
                                                    , [1,4], [1,2,4], [1,2], [1,2,3,4,6], [1..5], [3]
                                                    , [4], [1..6], [1,2,3,5,6], [1..4], [1..4], [1..6]
                                                    , 1:[3..6], [1..6], [2], [1..4], [1..4], [1..6]])

singleBoxGroupsTests = testGroup "singleBoxGroups"
  ( let rowOne = singleBoxGroups (map ((,) 0) [0..5]) [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)] singleBoxBoard
        rowTwo = singleBoxGroups (map ((,) 1) [0..5]) [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)] singleBoxBoard
    in
      [ testCase "(1,0)" $ getCell rowOne (1,0) @?= [2,6]
      , testCase "(1,2)" $ getCell rowOne (1,2) @?= [4,6]
      , testCase "all" $ rowOne @?= singleBoxBoardConstrained
      , testCase "all row two" $ rowTwo @?= singleBoxBoard
      ])

singleBoxLineTests = testGroup "singleBoxLine"
  ( let doRun = singleBoxLine singleBoxBoard (map ((,) 0) [0..5])
    in
      [ testCase "(1,0)" $ getCell doRun (1,0) @?= [2,6]
      , testCase "(1,2)" $ getCell doRun (1,2) @?= [4,6]
      , testCase "all" $ doRun @?= singleBoxBoardConstrained
      ])

singleBoxTests = testGroup "singleBox"
  [ testCase "all" $ singleBox singleBoxBoard @?= singleBoxBoardConstrained
  ]



forceParseBoard :: String -> Board
forceParseBoard = either (const (error "Failed to parse board")) id . parseBoard

disjointSubsetBoard :: Board
disjointSubsetBoard = forceParseBoard "3 3 \
                                      \1 9 2   _ _ 7   8 6 _ \
                                      \_ 6 7   _ _ 9   4 1 2 \
                                      \8 _ 4   _ _ _   7 _ 9 \

                                      \_ _ 9   _ 5 _   2 _ _ \
                                      \_ _ 6   7 _ 3   9 _ 1 \
                                      \_ 1 _   _ 9 _   6 4 _ \

                                      \9 _ _   _ _ _   1 2 8 \
                                      \_ 4 1   _ _ _   5 9 6 \
                                      \6 2 8   9 1 5   3 7 4 "

disjointSubsetTests = testGroup "disjointSubset"
  [ testCase "big scope" $ isSolved (execStrats [removeSolved, disjointSubset] disjointSubsetBoard) @?= True
  , testGroup "findDisjointSets"
    [ testCase "single" $ findDisjointSets [[1]] @?= [[1]]
    , testCase "2x2" $ findDisjointSets [[1,2], [1,2]] @?= [[1,2]]
    , testCase "2x2 out of order" $ findDisjointSets [[1,2], [2,1]] @?= [[1,2]]
    , testCase "2x2 + 1" $ findDisjointSets [[1,2], [3,4], [2,1]] @?= [[1,2]]
    , testCase "many" $ findDisjointSets [[1,2], [3], [4,5,6], [2,1], [6,5,4], [8,9], [5,6,4]] @?= [[1,2], [3], [4,5,6]]
    ]
  ]

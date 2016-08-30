module StrategiesSpec
  ( strategiesTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Sequence as Seq
import Data.List (foldl')

import Board
import Strategies
import Strategies.Internal

strategiesTests = testGroup "board tests"
  [ opOnGroupsTests
  , singlesOnlyTests
  , removeSolvedTests
  , fixSinglesTests
  , maybeFixTests
  , singleBoxGroupsTests
  , singleBoxLineTests
  , singleBoxTests
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
                                         , [1..6], [1..6], [1..6], [2], [4], [1]
                                         , [1..6], [1..6], [1..6], [1..6], [1..6], [3]
                                         , [4], [1..6], [1..6], [1..6], [1..6], [1..6]
                                         , [1..6], [1..6], [2], [1..6], [1..6], [1..6]])

singleBoxBoardConstrained = Board 3 2 (Seq.fromList [ [1,2], [3], [1,4], [5], [6], [2,4]
                                                    , [2,6], [5], [4,6], [1,4,3], [1,2,3], [2,4]
                                                    , [1..6], [1..6], [1..6], [2], [4], [1]
                                                    , [1..6], [1..6], [1..6], [1..6], [1..6], [3]
                                                    , [4], [1..6], [1..6], [1..6], [1..6], [1..6]
                                                    , [1..6], [1..6], [2], [1..6], [1..6], [1..6]])

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
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
  ]



prependIndices :: Board -> [Position] -> Board
prependIndices board@(Board m n contents) poss = foldl' prependIndex board (zip poss [0..])
  where
    prependIndex b (pos, i) = modify (i:) b pos



opOnGroupsTests = testGroup "opOnGroups"
  [ testCase "prepend index" $ opOnGroups prependIndices (Board 1 2 (Seq.fromList [[], [], [], []])) @?= Board 1 2 (Seq.fromList [[0, 0, 0], [1, 0, 0], [0, 1, 1], [1, 1, 1]])
  ]

singlesOnlyTests = testGroup "singlesOnly"
  [ testCase "single" $ singlesOnly [1] @?= [1]
  , testCase "zero" $ singlesOnly [] @?= ([] :: [Int])
  , testCase "three" $ singlesOnly [1,2,3] @?= []
  ]

removeSolvedTests = testGroup "removeSolved"
  [ testCase "in order" $ removeSolved (Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [1, 2]])) [(0, 0), (0, 1)] @?= Board 1 2 (Seq.fromList [[1], [2], [2, 1], [1, 2]])
  , testCase "out of order" $ removeSolved (Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [2, 1]])) [(0, 1), (1, 1)] @?= Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [1]])
  ]

fixSinglesTests = testGroup "fixSingles"
  [ testCase "in order" $ fixSingles (Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [1, 2]])) [(0, 0), (0, 1)] @?= Board 1 2 (Seq.fromList [[1], [2], [2, 1], [1, 2]])
  , testCase "out of order" $ fixSingles (Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [2, 1]])) [(0, 1), (1, 1)] @?= Board 1 2 (Seq.fromList [[1, 2], [2], [2, 1], [1]])
  ]

maybeFixTests = testGroup "maybeFix"
  [ testCase "empty toFix" $ maybeFix [] [1,2,3] @?= [1,2,3]
  , testCase "empty current" $ maybeFix [1,2] [] @?= []
  , testCase "single match" $ maybeFix [1] [2, 1, 3] @?= [1]
  , testCase "no match" $ maybeFix [1] [2,3,4] @?= [2,3,4]
  , testCase "double match" $ maybeFix [2,1] [3,1,2,4] @?= [1,2]
  ]

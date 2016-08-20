module BoardSpec
  ( boardTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Board (padString, groupN)

boardTests = testGroup "board tests" [padStringTests, groupNTests]



padStringTests = testGroup "padString"
  [ testCase "even addition" $ padString 3 "x" @?= " x "
  , testCase "even addition long" $ padString 8 "xxxx" @?= "  xxxx  "
  , testCase "odd addition" $ padString 4 "x" @?= " x  "
  , testCase "odd addition long" $ padString 9 "xx" @?= "   xx    "
  ]

groupNTests = testGroup "groupN"
  [ testCase "matched" $ groupN 2 [1..10] @?= [[1,2], [3,4], [5,6], [7,8], [9,10]]
  , testCase "zero length" $ length (groupN 0 [1,2,3]) @?= 0
  , testCase "zero input" $ length (groupN 1 []) @?= 0
  , testCase "unmatched" $ groupN 2 [1..5] @?= [[1,2], [3,4], [5]]
  ]
module BoardSpec
  ( boardTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Board

boardTests = testGroup "board tests" [padStringTests, groupNTests, showOptionsTests, showBoardTests]



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

showOptionsTests = testGroup "showOptions"
  [ testCase "empty" $ showOptions [] @?= ""
  , testCase "full" $ showOptions [1,2,3] @?= "1,2,3"
  ]

showBoardTests = testGroup "showBoard"
  [ testCase "zero" $ showBoard 0 0 [] @?= "┌┐\n\
                                           \││\n\
                                           \└┘"
  , testCase "1x1" $ showBoard 1 1 ["a"] @?= "┌─────┐\n\
                                             \│     │\n\
                                             \│  a  │\n\
                                             \│     │\n\
                                             \└─────┘"
  , testCase "3x2" $ showBoard 3 2 (map show [1..36]) @?= "┌──────────────┬──────────────┐\n\
                                                          \│              │              │\n\
                                                          \│  1   2   3   │  4   5   6   │\n\
                                                          \│              │              │\n\
                                                          \│  7   8   9   │  10  11  12  │\n\
                                                          \│              │              │\n\
                                                          \├──────────────┼──────────────┤\n\
                                                          \│              │              │\n\
                                                          \│  13  14  15  │  16  17  18  │\n\
                                                          \│              │              │\n\
                                                          \│  19  20  21  │  22  23  24  │\n\
                                                          \│              │              │\n\
                                                          \├──────────────┼──────────────┤\n\
                                                          \│              │              │\n\
                                                          \│  25  26  27  │  28  29  30  │\n\
                                                          \│              │              │\n\
                                                          \│  31  32  33  │  34  35  36  │\n\
                                                          \│              │              │\n\
                                                          \└──────────────┴──────────────┘"
  ]

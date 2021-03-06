module BoardSpec
  ( boardTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Sequence as Seq

import Sudoku.Board

boardTests = testGroup "board tests"
  [padStringTests
  , groupNTests
  , showOptionsTests
  , showBoardTests
  , rowsTests
  , colsTests
  , boxesTests
  , emptyTests
  , modifyTests
  , toIndexTests
  , getCellTests
  , isValidTests
  ]



padStringTests = testGroup "padString"
  [ testCase "even addition" $ padString 3 "x" @?= " x "
  , testCase "even addition long" $ padString 8 "xxxx" @?= "  xxxx  "
  , testCase "odd addition" $ padString 4 "x" @?= " x  "
  , testCase "odd addition long" $ padString 9 "xx" @?= "   xx    "
  ]

groupNTests = testGroup "groupN"
  [ testCase "matched" $ groupN 2 [1..10] @?= [[1,2], [3,4], [5,6], [7,8], [9,10]]
  , testCase "zero length" $ groupN 0 [1,2,3] @?= []
  , testCase "zero input" $ groupN 1 [] @?= ([] :: [[Int]])
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

rowsTests = testGroup "rows"
  [ testCase "0x0" $ rows 0 0 @?= []
  , testCase "1x2" $ rows 1 2 @?= [[(0, 0), (0, 1)], [(1, 0), (1, 1)]]
  , testCase "2x2" $ rows 2 2 @?= [[(0, 0), (0, 1), (0, 2), (0, 3)], [(1, 0), (1, 1), (1, 2), (1, 3)], [(2, 0), (2, 1), (2, 2), (2, 3)], [(3, 0), (3, 1), (3, 2), (3, 3)]]
  ]

colsTests = testGroup "cols"
  [ testCase "0x0" $ cols 0 0 @?= []
  , testCase "1x2" $ cols 1 2 @?= [[(0, 0), (1, 0)], [(0, 1), (1, 1)]]
  , testCase "2x2" $ cols 2 2 @?= [[(0, 0), (1, 0), (2, 0), (3, 0)], [(0, 1), (1, 1), (2, 1), (3, 1)], [(0, 2), (1, 2), (2, 2), (3, 2)], [(0, 3), (1, 3), (2, 3), (3, 3)]]
  ]

--  1x2                   2x1
-- ┌─────┬─────┐        ┌────────┐
-- │     │     │        │        │
-- │  1  │  2  │        │  1  2  │
-- │     │     │        │        │
-- │  2  │  1  │        ├────────┤
-- │     │     │        │        │
-- └─────┴─────┘        │  2  1  │
--                      │        │
--                      └────────┘

boxesTests = testGroup "boxes"
  [ testCase "0x0" $ boxes 0 0 @?= []
  , testCase "1x2" $ boxes 1 2 @?= [[(0, 0), (1, 0)], [(0, 1), (1, 1)]]
  , testCase "2x1" $ boxes 2 1 @?= [[(0, 0), (0, 1)], [(1, 0), (1, 1)]]
  , testCase "3x2 upper left" $ head (boxes 3 2) @?= [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)]
  , testCase "3x2 middle right" $ (boxes 3 2) !! 3 @?= [(2, 3), (2, 4), (2, 5), (3, 3), (3, 4), (3, 5)]
  ]

emptyTests = testGroup "empty"
  [ testCase "zero" $ empty 0 0 @?= Board 0 0 Seq.empty
  , testCase "1x2" $ empty 1 2 @?= Board 1 2 (Seq.fromList [[1,2], [1,2], [1,2], [1,2]])
  ]

modifyTests = testGroup "modify"
  [ testCase "cell 1" $ modify (const []) (empty 1 2) (0, 1) @?= Board 1 2 (Seq.fromList [[1,2], [], [1,2], [1,2]])
  ]

toIndexTests = testGroup "toIndex"
  [ testCase "zero" $ toIndex 0 0 (0, 0) @?= 0
  , testCase "row 0" $ toIndex 3 3 (0, 2) @?= 2
  , testCase "row 3" $ toIndex 3 2 (3, 4) @?= 22
  ]

getCellTests = testGroup "getCell"
  [ testCase "(0, 1)" $ getCell (Board 1 2 (Seq.fromList [[0], [1], [2], [3]])) (0, 1) @?= [1]
  ]

isValidTests = testGroup "isValid"
  [ testCase "good" $ isValid (Board 1 2 (Seq.fromList [[1], [2], [1], [2]])) @?= True
  , testCase "bad" $ isValid (Board 1 2 (Seq.fromList [[1,2], [1], [2], []])) @?= False
  ]
